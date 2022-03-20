{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
-- import qualified Data.Text as T

import Template
import Util

data Uniform = UF String
  deriving (Eq, Show, Read, Ord)

data E = KF Double | U Uniform | Add E E | Sub E E | Mul E E | Div E E | Length E | V2 E E | XY | Sh E | ShRef Int
       | Abs E | Min E E | Max E E | X E | Y E | Neg E | Fun1 String Ty Ty E | Mat2 [E]
  deriving (Eq, Show, Read, Ord)

infixl 6 +.
(+.) = Add
infixl 6 -.
(-.) = Sub
infixl 7 *.
(*.) = Mul
infixl 7 /.
(/.) = Div

type Refs = M.Map Int E
type RevRefs = M.Map E Int
type RefState = (Int, Refs, RevRefs)
type Sharey a = State RefState a

initState :: RefState
initState = (0, M.empty, M.empty)

data Ty = TF | TV2 | TM2
  deriving (Eq, Show, Read, Ord)

typeOf :: Refs -> E -> Ty
typeOf refs (KF _) = TF
typeOf refs (U (UF _)) = TF
typeOf refs (Add a b) = opType refs a b
typeOf refs (Sub a b) = opType refs a b
typeOf refs (Mul a b) = opType refs a b
typeOf refs (Div a b) = opType refs a b
typeOf refs (Length e) = mustType refs e [TV2] TF
typeOf refs (V2 _ _) = TV2
typeOf refs XY = TV2
typeOf refs (ShRef n) = typeOf refs (refs M.! n)
typeOf refs (Abs e) = typeOf refs e
typeOf refs (Min a b) = sameType refs a b
typeOf refs (Max a b) = sameType refs a b
typeOf refs (X e) = TF
typeOf refs (Y e) = TF
typeOf refs (Neg e) = mustType refs e [TF] TF
typeOf refs (Fun1 name tin tout arg) = mustType refs arg [tin] tout
typeOf refs (Mat2 _) = TM2

glslType :: Ty -> String
glslType TF = "float"
glslType TV2 = "vec2"
glslType TM2 = "mat2"

-- Figure out type of the overloaded arithmetic ops
opType :: Refs -> E -> E -> Ty
opType refs a b = go (typeOf refs a) (typeOf refs b)
  where go TF TF = TF
        go TV2 TF = TV2
        go TF TV2 = TV2
        go TV2 TV2 = TV2
        go TM2 TV2 = TV2
        go a b = error $ "opType? " ++ show a ++ " " ++ show b

-- The expression's type must be in the list; returns the last arg
mustType :: Refs -> E -> [Ty] -> Ty -> Ty
mustType refs e tys ty | (typeOf refs e) `elem` tys = ty
                  | otherwise = error $ "wrong type " ++ show e ++ " " ++ show tys

-- Types must be the same; returns the type
sameType :: Refs -> E -> E -> Ty
sameType refs a b | typeOf refs a == typeOf refs b = typeOf refs a
             | otherwise = error $ "type mismatch: " ++ show a ++ " " ++ show b

share :: E -> (E, Refs)
share e =
  let (e', (_, refs, revRefs)) = runState (share' e) initState
   in (e', refs)

share' :: E -> State RefState E
share' (Sh e) = do
  e' <- share' e
  (n, refs, revRefs) <- get
  case revRefs M.!? e' of
    Just n -> return $ ShRef n
    Nothing -> do
      let refs' = M.insert n e' refs
      let revRefs' = M.insert e' n revRefs
      put (n + 1, refs', revRefs')
      return $ ShRef n
share' (Add a b) = do
  a' <- share' a
  b' <- share' b
  return $ Add a' b'
share' (Sub a b) = do
  a' <- share' a
  b' <- share' b
  return $ Sub a' b'
share' (Mul a b) = do
  a' <- share' a
  b' <- share' b
  return $ Mul a' b'
share' (Div a b) = do
  a' <- share' a
  b' <- share' b
  return $ Div a' b'
share' (Length e) = do
  e' <- share' e
  return $ Length e'
share' (V2 a b) = do
  a' <- share' a
  b' <- share' b
  return $ V2 a' b'
share' (Abs e) = do
  e' <- share' e
  return $ Abs e'
share' (Min a b) = do
  a' <- share' a
  b' <- share' b
  return $ Min a' b'
share' (Max a b) = do
  a' <- share' a
  b' <- share' b
  return $ Max a' b'
share' (X e) = do
  e' <- share' e
  return $ X e'
share' (Y e) = do
  e' <- share' e
  return $ Y e'
share' (Neg e) = do
  e' <- share' e
  return $ Neg e'
share' (Fun1 name tin tout e) = do
  e' <- share' e
  return $ Fun1 name tin tout e'
share' (Mat2 es) = do
  es' <- mapM share' es
  return $ Mat2 es'
share' x = return x

subexp :: Int -> String
subexp n = "x" ++ show n

parens :: String -> String
parens x = concat ["(", x, ")"]

op :: String -> String -> String -> String
op operator a b = parens $ concat [parens a, operator, parens b]

fun :: String -> [String] -> String
fun f args = parens $ concat [f, parens arglist]
  where arglist = intercalate ", " args

dot :: String -> String -> String
dot e field = parens $ concat [e, ".", field]

compileE :: E -> String
compileE (KF d) = parens $ show d
compileE (U (UF name)) = parens name
compileE (Add a b) = op "+" (compileE a) (compileE b)
compileE (Sub a b) = op "-" (compileE a) (compileE b)
compileE (Mul a b) = op "*" (compileE a) (compileE b)
compileE (Div a b) = op "/" (compileE a) (compileE b)
compileE (Length e) = fun "length" [compileE e]
compileE (V2 a b) = fun "vec2" [compileE a, compileE b]
compileE XY = "(uv)"
compileE e@(Sh _) = error $ "Can't compile Sh nodes: " ++ show e
compileE (ShRef n) = parens $ subexp n
compileE (Abs e) = fun "abs" [compileE e]
compileE (Min a b) = fun "min" [compileE a, compileE b]
compileE (Max a b) = fun "max" [compileE a, compileE b]
compileE (X e) = dot (compileE e) "x"
compileE (Y e) = dot (compileE e) "y"
compileE (Neg e) = parens $ concat ["-", compileE e]
compileE (Fun1 name _ _ arg) = fun name [compileE arg]
compileE (Mat2 es) = fun "mat2" (map compileE es)

compileBinding :: Refs -> String -> E -> String
compileBinding refs var e = concat [ty, " ", var, " = ", compileE e]
  where ty = glslType $ typeOf refs e

compileBindings :: Refs -> [(String, E)] -> String
compileBindings refs bindings = intercalate ";\n" cbs ++ ";\n"
  where cbs = map (\(var, e) -> compileBinding refs var e) bindings

compileGroup :: (E, Refs) -> String -> String
compileGroup (top, refs) topName = compileBindings refs bindings
  where bindings =  shares ++ [(topName, top)]
        shares = map (\(n, e) -> (subexp n, e)) (M.toList refs)

ssqrt = Fun1 "sqrt" TF TF
ssin = Fun1 "sin" TF TF
scos = Fun1 "cos" TF TF

circle =
  let yeah = Sh $ U (UF "yeah")
      center = Sh $ V2 (KF 0.2 +. yeah) (KF 0.2)
      radius = Sh $ KF 0.2
      dist = (Length (XY -. center) /. radius) -. KF 1.0
   in dist

square =
  let center = Sh $ V2 (KF 0.0) (KF 0.0)
      radius = Sh $ KF 0.2
      sd = Sh $ Abs (XY -. center)
      dist = Sh $ (Max (X sd) (Y sd) /. radius) -. KF 1.0
   in dist

tsquare :: E -> E -> E
tsquare xy _ =
  let center = Sh $ V2 (KF 0.0) (KF 0.0)
      radius = Sh $ KF 0.2
      sd = Sh $ Abs (xy -. center)
      dist = Sh $ (Max (X sd) (Y sd) /. radius) -. KF 1.0
   in dist

rotMat :: E -> E
rotMat ang =
  let c = scos ang
      s = ssin ang
      mat = Mat2 [c, s, Neg s, c]
   in mat

union a b = Min a b
intersection a b = Max a b
difference a b = Max a (Neg b)

smoothUnion :: E -> E -> E
smoothUnion usd0 usd1 =
  let d0 = Sh usd0
      d1 = Sh usd1
      r = KF 0.3
      md0 = Sh $ Min (d0 -. r) (KF 0.0)
      md1 = Sh $ Min (d1 -. r) (KF 0.0)
      inside_distance = Neg $ ssqrt $ (md0 *. md0) +. (md1 *. md1)
      simple_union = Min d0 d1
      outside_distance = Max simple_union r
      dist = inside_distance +. outside_distance
   in dist

smu = smoothUnion circle square

scale :: E -> Transformer
scale s (Transform xy t) = Transform (xy /. s) t

translation :: E -> Transformer
translation dxy (Transform xy t) = Transform (xy -. dxy) t

rotation :: E -> Transformer
rotation ang (Transform xy t) =
  let c = scos ang
      s = ssin ang
      mat = Sh $ Mat2 [c, s, Neg s, c]
   in Transform (mat *. xy) t

-- Transform uv t
data Transform = Transform E E
type Transformer = Transform -> Transform
type Prim = Transform -> E

transform :: Transformer -> Prim -> Prim
transform transformer p = p . transformer

psquare :: Transform -> E
psquare (Transform xy _) =
  let center = Sh $ V2 (KF 0.0) (KF 0.0)
      radius = Sh $ KF 0.2
      sd = Sh $ Abs (xy -. center)
      dist = Sh $ (Max (X sd) (Y sd) /. radius) -. KF 1.0
   in dist

pcircle :: Transform -> E
pcircle (Transform xy _) =
  let dist = Length xy -. KF 1.0
   in dist

idTransform :: Transform
idTransform = Transform XY tyme
tyme :: E
tyme = (U (UF "yeah"))

evalPrim :: Prim -> E
evalPrim p = p idTransform

main = do
  let t = U (UF "yeah")
  -- let rotXY = rotMat t *. XY

  -- let rot = rotation $ KF 50.0 *. t
  --     slide = translation (V2 (t *. KF 0.2) (KF 0.0))
  --     -- p = transform rot psquare
  --     srp = transform rot $ transform slide psquare
  --     rsp = transform slide $ transform rot psquare
  --     s = smoothUnion (evalPrim srp) (evalPrim rsp)

  -- let s = tsquare (XY /. t) t
  -- let s = tsquare rotXY t
  -- let s = smu
  let cir = transform (translation (V2 (t *. KF 0.8) (KF 0.0))) $ transform (scale $ KF 0.15) pcircle
      smaller = transform (translation (V2 (t *. KF 0.8) (KF 0.0))) $ transform (scale $ KF 0.03) pcircle
      both = smoothUnion (evalPrim psquare) (evalPrim cir)
      s' = difference both (evalPrim cir)
      s = union s' (evalPrim smaller)
  let c = compileGroup (share s) "dist"
  msp c
  generateExe "template.html" "index.html" $ M.fromList [("SHAPE_ASDF", c)]
  msp "hi"
