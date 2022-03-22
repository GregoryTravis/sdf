{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
-- import qualified Data.Text as T

import Template
import Util hiding (time)

data Uniform = UF String
  deriving (Eq, Show, Read, Ord)

data E = KF Double | U Uniform | Add E E | Sub E E | Mul E E | Div E E | Length E | V2 E E | XY | Sh E | ShRef Int
       | Abs E | Min E E | Max E E | X E | Y E | Neg E | Fun1 String Ty Ty E | Fun2 String Ty Ty Ty E E | Mat2 [E]
       | Equals E E | Cond E E E
  deriving (Eq, Show, Read, Ord)

instance Num E where
  (+) = Add
  (*) = Mul
  abs = Abs
  signum = error "signum not implemented"
  fromInteger i = KF (fromInteger i)
  negate = Neg

-- fromRational, (recip | (/))
instance Fractional E where
  fromRational i = KF (fromRational i)
  (/) = Div

-- instance Eq E where
--   (==) = Equals
infix 4 ==.
(==.) = Equals

type Refs = M.Map Int E
type RevRefs = M.Map E Int
type RefState = (Int, Refs, RevRefs)
type Sharey a = State RefState a

initState :: RefState
initState = (0, M.empty, M.empty)

data Ty = TF | TV2 | TM2 | TB
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
typeOf refs (Neg e) = mustType refs e [TF, TV2] TF
typeOf refs (Fun1 name tin tout arg) = mustType refs arg [tin] tout
typeOf refs (Fun2 name tin tin2 tout arg0 arg1) = tout -- should check both
typeOf refs (Mat2 _) = TM2
typeOf refs (Equals _ _) = TB
typeOf refs (Cond _ t e) = sameType refs t e

glslType :: Ty -> String
glslType TF = "float"
glslType TV2 = "vec2"
glslType TM2 = "mat2"
glslType TB = "bool"

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
share' (Fun2 name tin tin2 tout e0 e1) = do
  e0' <- share' e0
  e1' <- share' e1
  return $ Fun2 name tin tin2 tout e0' e1'
share' (Mat2 es) = do
  es' <- mapM share' es
  return $ Mat2 es'
share' (Equals a b) = do
  a' <- share' a
  b' <- share' b
  return $ Equals a' b'
share' (Cond b t e) = do
  b' <- share' b
  t' <- share' t
  e' <- share' e
  return $ Cond b' t' e'
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

cond :: String -> String -> String -> String
cond b t e = parens $ concat [parens b, "?", parens t , ":", parens e]

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
compileE (Fun2 name _ _ _ arg0 arg1) = fun name [compileE arg0, compileE arg1]
compileE (Mat2 es) = fun "mat2" (map compileE es)
compileE (Equals a b) = op "==" (compileE a) (compileE b)
compileE (Cond b t e) = cond (compileE b) (compileE t) (compileE e)

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

sabs = Abs
smod = Fun2 "mod" TF TF TF
sfloor = Fun1 "floor" TF TF

rotMat :: E -> E
rotMat ang =
  let c = Sh $ scos ang
      s = Sh $ ssin ang
      mat = Mat2 [c, s, -s, c]
   in mat

union :: BinOp
union = binopper union'
union' :: E -> E -> E
union' a b = Min a b

intersection :: BinOp
intersection = binopper intersection'
intersection' :: E -> E -> E
intersection' a b = Max a b

difference :: BinOp
difference = binopper difference'
difference' :: E -> E -> E
difference' a b = Max a (- b)

binopper :: (E -> E -> E) -> BinOp
binopper distCombiner p0 p1 tr = distCombiner (Sh $ p0 tr) (Sh $ p1 tr)

smoothUnion :: BinOp
smoothUnion = binopper smoothUnion'

smoothUnion' :: E -> E -> E
smoothUnion' usd0 usd1 =
  let d0 = Sh usd0
      d1 = Sh usd1
      r = 0.3
      md0 = Sh $ Min (d0 - r) 0.0
      md1 = Sh $ Min (d1 - r) 0.0
      inside_distance = - (ssqrt $ (md0 * md0) + (md1 * md1))
      simple_union = Min d0 d1
      outside_distance = Max simple_union r
      dist = inside_distance + outside_distance
   in dist

scale :: E -> UnOp
scale s = transform (scale' s)

scale' :: E -> Transformer
scale' s (Transform xy t) = Transform (xy / s) t

translation :: E -> UnOp
translation dxy = transform (translation' dxy)

translation' :: E -> Transformer
translation' dxy (Transform xy t) = Transform (xy - dxy) t

rotation :: E -> UnOp
rotation ang = transform (rotation' ang)

rotation' :: E -> Transformer
rotation' ang (Transform xy t) =
  let c = Sh $ scos ang
      s = Sh $ ssin ang
      mat = Sh $ Mat2 [c, s, -s, c]
   in Transform (mat * xy) t

grid :: E -> E -> UnOp
grid w h = transform (grid' w h)

grid' :: E -> E -> Transformer
grid' w h (Transform xy t) =
  let x = Sh $ X xy
      y = Sh $ Y xy
      xx = Sh $ smod x w
      yy = Sh $ smod y h
      xi = Sh $ sfloor x
      yi = Sh $ sfloor y
   in Transform (V2 xx yy) t

bugPfGrid :: E -> E -> UnOp
bugPfGrid w h = transform (bugPfGrid' w h)

bugPfGrid' :: E -> E -> Transformer
bugPfGrid' w h (Transform xy t) =
  let x = X xy
      y = Y xy
      xx = smod x w
      yy = smod y h
      xi = sfloor x
      yi = sfloor y
      xx2 = Cond (smod (Abs xi) 2 ==. 1) (w - xx) xx
      yy2 = Cond (smod (Abs yi) 2 ==. 1) (h - yy) yy
   in Transform (V2 xx2 yy2) t

bugPfGrid2 :: E -> E -> UnOp
bugPfGrid2 w h = transform (bugPfGrid2' w h)

bugPfGrid2' :: E -> E -> Transformer
bugPfGrid2' w h (Transform xy t) =
  let x = X xy
      y = Y xy
      xx = smod x w
      yy = smod y h
      xi = sfloor x
      yi = sfloor y
      xx2 = Cond (smod xi 2 ==. 1) (w - xx) xx
      yy2 = Cond (smod yi 2 ==. 1) (h - yy) yy
   in Transform (V2 xx2 yy2) t

pfGrid :: E -> E -> UnOp
pfGrid w h = transform (pfGrid' w h)

-- I tried using mod to calculate xx, yy, xi, yi, but it was wrong, so I just inlined the original rust grid_fmod2()
pfGrid' :: E -> E -> Transformer
pfGrid' w h (Transform xy t) =
  let x = X xy
      y = Y xy
      xow = Sh $ x / w
      yoh = Sh $ y / h
      xx = Sh $ (xow - sfloor xow) * w
      xi = Sh $ sfloor xow
      yy = Sh $ (yoh - sfloor yoh) * h
      yi = Sh $ sfloor yoh
      xx2 = Sh $ Cond (smod (Abs xi) 2.0 ==. 1.0) (w - xx) xx
      yy2 = Sh $ Cond (smod (Abs yi) 2.0 ==. 1.0) (h - yy) yy
   in Transform (V2 xx2 yy2) t

-- fn grid_fmod2(a: f32, b: f32) -> (f32, i32) {
--   let aob = a / b;
--   (((aob - aob.floor()) * b), aob.floor() as i32)
-- }

    -- let (mut xx, xi) = grid_fmod2(x, w);
    -- let (mut yy, yi) = grid_fmod2(y, h);
    -- if xi.abs() % 2 == 1 {
    --   xx = w - xx;
    -- }
    -- if yi.abs() % 2 == 1 {
    --   yy = h - yy;
    -- }
    -- (xx, yy, t)

-- Transform uv t
data Transform = Transform E E
type Transformer = Transform -> Transform
type Prim = Transform -> E
type UnOp = Prim -> Prim
type BinOp = Prim -> Prim -> Prim

transform :: Transformer -> Prim -> Prim
transform transformer p = p . transformer

square :: Prim
square (Transform xy _) =
  let center = Sh $ V2 0.0 0.0
      radius = Sh 1.0
      sd = Sh $ Abs (xy - center)
      dist = Sh $ (Max (X sd) (Y sd) / radius) - 1.0
   in dist

circle :: Prim
circle (Transform xy _) =
  let dist = Length xy - 1.0
   in dist

idTransform :: Transform
idTransform = Transform XY time

time :: E
time = (U (UF "yeah"))

evalPrim :: Prim -> E
evalPrim p = p idTransform

main = do
  let camera = scale 0.1

  let rot = rotation $ 50.0 * time
      slide = translation (V2 (time * 0.8) 0.0)
      -- p = transform rot square
      srp = rot $ slide square
      rsp = slide $ rot square
      p2 = smoothUnion srp rsp

  -- let s = tsquare (XY / t) t
  -- let s = tsquare rotXY t
  -- let s = smu

  let cir = (translation (V2 (- (time * 0.8)) 0.0)) $ (scale 0.15) circle
      smaller = (translation (V2 (- (time * 0.8)) 0.0)) $ (scale 0.03) circle
      both = smoothUnion square cir
      p' = difference both cir
      p3 = union p' smaller

  -- let p = union p2 p3
  -- let p = pfGrid 2.25 2.25 square

  -- FAVORITE don't lose this!!
  -- fall in love all over again
  let filaoa' = pfGrid 2.25 2.25 circle
  let filaoa = smoothUnion (scale time filaoa') (rotation time filaoa')

  let p = filaoa

  let pc = camera p

  let s = evalPrim pc

  let c = compileGroup (share s) "dist"
  msp c
  generateExe "template.html" "index.html" $ M.fromList [("SHAPE_ASDF", c)]
  msp "hi"
