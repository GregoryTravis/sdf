{-# LANGUAGE GADTs #-}

module Compile
( compileSingle
, compileFunction ) where

import Data.List (intercalate)
import qualified Data.Map as M

import E
import Share
import Transform

typeOf :: Refs -> E -> Ty
typeOf refs (KF _) = TF
typeOf refs (U (UF _)) = TF
typeOf refs (Add a b) = opType refs a b
typeOf refs (Sub a b) = opType refs a b
typeOf refs (Mul a b) = opType refs a b
typeOf refs (Div a b) = opType refs a b
typeOf refs (Length e) = mustType refs e [TV2] TF
typeOf refs (V2 _ _) = TV2
typeOf refs (V3 _ _ _) = TV3
typeOf refs (V4 _ _ _ _) = TV4
typeOf refs XY = TV2
typeOf refs Mouse = TV2
typeOf refs (ShareRef n) = typeOf refs (refs M.! n)
typeOf refs (Abs e) = typeOf refs e
typeOf refs (Min a b) = sameType refs a b
typeOf refs (Max a b) = sameType refs a b
typeOf refs (X e) = TF
typeOf refs (Y e) = TF
typeOf refs (Neg e) = mustType refs e [TF, TV2] TF
typeOf refs (Fun1 name tin tout arg) = mustType refs arg [tin] tout
typeOf refs (Fun2 name tin tin2 tout arg0 arg1) = tout -- should check both
typeOf refs (Fun name tins tout args) = tout -- should check both
typeOf refs (Mat2 _) = TM2
typeOf refs (Comparison _ _ _) = TB
typeOf refs (Cond _ t e) = sameType refs t e
typeOf refs (RGB e) = TV3
typeOf refs (A e) = TF
-- typeOf refs (Var name) = sameType refs t e

glslType :: Ty -> String
glslType TF = "float"
glslType TV2 = "vec2"
glslType TV3 = "vec3"
glslType TV4 = "vec4"
glslType TM2 = "mat2"
glslType TB = "bool"

-- Figure out type of the overloaded arithmetic ops
opType :: Refs -> E -> E -> Ty
opType refs a b = go (typeOf refs a) (typeOf refs b)
  where go TF TF = TF
        go TV2 TF = TV2
        go TF TV2 = TV2
        go TV2 TV2 = TV2
        go TV3 TF = TV3
        go TF TV3 = TV3
        go TV3 TV3 = TV3
        go TV4 TF = TV4
        go TF TV4 = TV4
        go TV4 TV4 = TV4
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

-- It's assume the top level of any call to compileE is a call to parens
compileSubE :: Show a => E a -> String
compileSubE (KF n) = parens $ show n
compileSubE (Add a b) = op "+" (compileSubE a) (compileSubE b)
compileSubE (Sub a b) = op "-" (compileSubE a) (compileSubE b)
compileSubE (Mul a b) = op "*" (compileSubE a) (compileSubE b)
compileSubE (Div a b) = op "/" (compileSubE a) (compileSubE b)
compileSubE (V2 a b) = fun "vec2" [compileSubE a, compileSubE b]
compileSubE (V3 a b c) = fun "vec3" [compileSubE a, compileSubE b, compileSubE c]
compileSubE (V4 a b c d) = fun "vec4" [compileSubE a, compileSubE b, compileSubE c, compileSubE d]
compileSubE (Length e) = fun "length" [compileSubE e]
compileSubE (Uniform name) = parens name
compileSubE (Swizzle swizzler v) = dot (compileSubE v) (swizzleFields swizzler)
compileSubE (Field (Fielder field) v) = dot (compileSubE v) field
compileSubE (Fun1 name arg1) = fun name [compileSubE arg1]
compileSubE (Fun2 name arg1 arg2) = fun name [compileSubE arg1, compileSubE arg2]
compileSubE (Fun3 name arg1 arg2 arg3) = fun name [compileSubE arg1, compileSubE arg2, compileSubE arg3]
compileSubE (Neg a) = parens $ concat ["-", compileSubE a]
compileSubE (Mat2 xs) = fun "mat2" (map compileSubE xs)
compileSubE (Comparison name a b) = op name (compileSubE a) (compileSubE b)
compileSubE (Cond b t e) = cond (compileSubE b) (compileSubE t) (compileSubE e)
compileSubE e@(Share _ _) = error $ "Can't compile Sh nodes: " ++ show e
-- compileSubE (Share _ e) = compileSubE e
compileSubE (ShareRef n) = parens $ subexp n
compileSubE e = error $ "compileSubE: " ++ show e

-- Compile an expression to a list of bindings, with the specified name for the
-- outermost expression binding.
compileE :: (Show a, GlslType a) => String -> E a -> String
compileE topVar e = renderBindings (compileEToBindings topVar e)

compileEToBindings :: (Show a, GlslType a) => String -> E a -> [(String, String, String)]
compileEToBindings topVar e =
  let (e', (_, seMap)) = runState (share' e) initState
      compiledE = compileSubE e'
      ty = typeName e'
  in toBindings seMap ++ [(topVar, ty, compiledE)]

renderBindings :: [(String, String, String)] -> String
renderBindings bindings = intercalate ";\n" lines ++ ";\n"
  where lines = map renderBinding bindings

renderBinding :: (String, String, String) -> String
renderBinding (var, ty, se) = concat [ty, " ", var, " = ", se]

toBindings :: SEMap -> [(String, String, String)]
toBindings seMap = map f sortedElems
  where f (n, ty, se) = (subexp n, ty, se)
        sortedElems = sortOn fst3 (HM.elems seMap)
        fst3 (a, _, _) = a

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

compileSingle :: E -> String
compileSingle e = compileGroup (share e) "topColor"

-- Should add return, functions, etc to the DSL
compileFunction :: E -> String
compileFunction e =
  let top = "toppppp"
      bindings = compileGroup (share e) top
      -- all = "vec4 " ++ name ++ "() {\n" ++ bindings ++ "return " ++ top ++ ";\n}\n"
      all = bindings ++ "return " ++ top ++ ";\n"
   in all

data DSN where
  DSN :: StableName (E a) -> DSN

instance Hashable DSN where
  -- hashWithSalt :: Int -> a -> Int
  hashWithSalt salt (DSN sn) = (salt * 10) + (hashStableName sn)

instance Eq DSN where
  DSN sn0 == DSN sn1 = eqStableName sn0 sn1

type SEMap = HM.HashMap DSN (Int, String, String)

type ShareState = (Int, SEMap)

initState :: ShareState
initState = (0, HM.empty)

addSE :: (Show a, GlslType a) => (StableName (E a)) -> E a -> State ShareState ()
addSE sn se = do
  (n, seMap) <- get
  let n' = n + 1
      cse = compileSubE se
      ty = typeName se
      seMap' = HM.insert (DSN sn) (n, ty, cse) seMap
  put (n', seMap')

hasSE :: (StableName (E a)) -> State ShareState Bool
hasSE sn = do
  (_, seMap) <- get
  return $ HM.member (DSN sn) seMap

getRef :: (StableName (E a)) -> State ShareState Int
getRef sn = do
  (_, seMap) <- get
  case HM.lookup (DSN sn) seMap of
    Just (n, _, _) -> return n
    Nothing -> error "missing from refMap"

-- -- Extract Sh nodes from e and store them in the map.
-- share :: E -> (E, Refs)
-- share e =
--   let (e', (_, capMap, refMap)) = runState (share' e) initState
--       refs = toRefs capMap refMap
--    in (e', refs)

-- -- Assign a unique small integer to each sn. Maybe I could use hashStableName
-- -- but it's not clear how unlikely that is to have a collision
-- toRefs :: CapMap -> RefMap -> Refs
-- toRefs capMap refMap =
--   let sns = HM.keys capMap
--       refs = Prelude.map (\sn -> refMap HM.! sn) sns
--       caps = Prelude.map (\sn -> capMap HM.! sn) sns
--    in M.fromList (zip refs caps)

-- Allocate a fresh tag n for expression e, add (n -> e') to the map state,
-- where e' is the result of the recursive call to share' on e.
-- Return (ShRef n).
share' :: (Show a, GlslType a) => E a -> State ShareState (E a)
share' (Share sn e) = do
  hc <- hasSE sn
  if hc
    then do n <- getRef sn
            return $ ShareRef n
    else do e' <- share' e
            addSE sn e'
            n <- getRef sn
            return $ ShareRef n
share' e@(KF _) = return e
share' e@(KD _) = return e
share' e@(KI _) = return e
share' (V2 a b) = do
  a' <- share' a
  b' <- share' b
  return $ V2 a' b'
share' (V3 a b c) = do
  a' <- share' a
  b' <- share' b
  c' <- share' c
  return $ V3 a' b' c'
share' (V4 a b c d) = do
  a' <- share' a
  b' <- share' b
  c' <- share' c
  d' <- share' d
  return $ V4 a' b' c' d'
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
share' e@(Uniform _) = return e
share' (Swizzle swizzler e) = do
  e' <- share' e
  return $ Swizzle swizzler e'
share' (Field fielder e) = do
  e' <- share' e
  return $ Field fielder e'
share' (Fun1 name a) = do
  a' <- share' a
  return $ Fun1 name a'
share' (Fun2 name a b) = do
  a' <- share' a
  b' <- share' b
  return $ Fun2 name a' b'
share' (Fun3 name a b c) = do
  a' <- share' a
  b' <- share' b
  c' <- share' c
  return $ Fun3 name a' b' c'
share' (Neg e) = do
  e' <- share' e
  return $ Neg e'
share' (Mat2 es) = do
  es' <- mapM share' es
  return $ Mat2 es'
share' (Comparison op a b) = do
  a' <- share' a
  b' <- share' b
  return $ Comparison op a' b'
share' (Cond b t e) = do
  b' <- share' b
  t' <- share' t
  e' <- share' e
  return $ Cond b' t' e'
share' x = error $ "share': " ++ show x
