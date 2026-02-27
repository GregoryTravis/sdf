{-# LANGUAGE GADTs #-}

module Compile
(   compileBinding
  , compileFunction
  , compileSingle
) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as M
import System.Mem.StableName

import E
import Transform

subexp :: Int -> String
subexp n = "x" ++ show n

parens :: String -> String
parens x = concat ["(", x, ")"]

op :: String -> String -> String -> String
op operator a b = parens $ concat [parens a, operator, parens b]

fun :: String -> [String] -> String
fun f args = parens $ concat [f, parens arglist]
  where arglist = intercalate ", " args

method :: String -> [String] -> String
method f [] = error $ "Method must have at least a 'this' arg: " ++ f
method f (this:args) = parens $ concat [parens this, ".", f, parens arglist]
  where arglist = intercalate ", " args

dot :: String -> String -> String
dot e field = parens $ concat [e, ".", field]

cond :: String -> String -> String -> String
cond b t e = parens $ concat [parens b, "?", parens t , ":", parens e]

-- It's assume the top level of any call to compileE is a call to parens
compileSubE :: Show a => E a -> String
compileSubE (KI i) = parens $ show i
compileSubE (KF n) = parens $ show n
compileSubE (Add a b) = op "+" (compileSubE a) (compileSubE b)
compileSubE (Sub a b) = op "-" (compileSubE a) (compileSubE b)
compileSubE (Mul a b) = op "*" (compileSubE a) (compileSubE b)
compileSubE (Div a b) = op "/" (compileSubE a) (compileSubE b)
compileSubE (And a b) = op "&&" (compileSubE a) (compileSubE b)
compileSubE (Or a b) = op "||" (compileSubE a) (compileSubE b)
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
compileSubE (Method0 name this) = method name [compileSubE this]
compileSubE (Neg a) = parens $ concat ["-", compileSubE a]
compileSubE (Mat2 xs) = fun "mat2" (map compileSubE xs)
compileSubE e@(Arr xs) = fun (typeName e) (map compileSubE xs)
compileSubE (ArrLookup arr index) =
  let arrC = parens (compileSubE arr)
      indexC = parens (compileSubE index)
   in parens $ arrC ++ "[" ++ indexC ++ "]"
compileSubE (Comparison name a b) = op name (compileSubE a) (compileSubE b)
compileSubE (Cond b t e) = cond (compileSubE b) (compileSubE t) (compileSubE e)
compileSubE e@(Share _ _) = error $ "Can't compile Sh nodes: " ++ show e
-- compileSubE (Share _ e) = compileSubE e
compileSubE (ShareRef n) = parens $ subexp n
compileSubE (Tap e _) = compileSubE e
compileSubE e = error $ "compileSubE: " ++ show e

-- Compile an expression to a list of bindings, with the specified name for the
-- outermost expression binding.
compileE :: (Show a, GlslType a) => String -> E a -> String
compileE topVar e = renderBindings (compileEToBindings topVar e)

compileEToBindings :: (Show a, GlslType a) => String -> E a -> [(String, String, String)]
compileEToBindings topVar e =
  let (e', seMap) = share e -- runState (share' e) initState
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

-- public
-- compileBinding :: Refs -> String -> E a -> String
-- compileBinding refs var e = concat [ty, " ", var, " = ", compileE e]
--   where ty = glslType $ typeOf refs e
compileBinding :: (Show a, GlslType a) => String -> E a -> String
compileBinding var e = compileE var e
-- compileBinding var e = concat [ty, " ", var, " = ", compileE e, ";"]
--   where ty = typeName e

-- -- private
-- compileBindings :: SEMap -> [(String, E a)] -> String
-- compileBindings refs bindings = intercalate ";\n" cbs ++ ";\n"
--   where cbs = map (\(var, e) -> compileBinding refs var e) bindings

-- -- private
-- compileGroup :: (E,  SEMap) -> String -> String
-- compileGroup (top, semap) topName = compileBindings semap bindings
--   where bindings = shares ++ [(topName, typeOf top, top)]
--         shares = map (\(n, ty, e) -> (subexp n, ty, e)) (M.toList semap)

compileSingle :: (Show a, GlslType a) => E a -> String
compileSingle = compileBinding "topColor"
-- compileSingle e = compileGroup (share e) "topColor"

-- Should add return, functions, etc to the DSL
-- public
compileFunction :: (Show a, GlslType a) => E a -> String
compileFunction = compileBinding "toppppp"
-- compileFunction e =
--   let top = "toppppp"
--       bindings = compileGroup (share e) top
--       -- all = "vec4 " ++ name ++ "() {\n" ++ bindings ++ "return " ++ top ++ ";\n}\n"
--       all = bindings ++ "return " ++ top ++ ";\n"
--    in all

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

-- Extract Sh nodes from e and store them in the map.
share :: (Show a, GlslType a) => E a -> (E a, SEMap)
share e =
  let (e', (_, semap)) = runState (share' e) initState
   in (e', semap)

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
  -- In this case we only recurse into `e` if we haven't already seen this node.
  hc <- hasSE sn
  if hc
    -- TODO factor out here
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
share' (And a b) = do
  a' <- share' a
  b' <- share' b
  return $ And a' b'
share' (Or a b) = do
  a' <- share' a
  b' <- share' b
  return $ Or a' b'
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
share' (Method0 name a) = do
  a' <- share' a
  return $ Method0 name a'
share' (Neg e) = do
  e' <- share' e
  return $ Neg e'
share' (Mat2 es) = do
  es' <- mapM share' es
  return $ Mat2 es'
share' (Arr es) = do
  es' <- mapM share' es
  return $ Arr es'
share' (ArrLookup arr index) = do
  arr' <- share' arr
  index' <- share' index
  return $ ArrLookup arr' index'
share' (Comparison op a b) = do
  a' <- share' a
  b' <- share' b
  return $ Comparison op a' b'
share' (Cond b t e) = do
  b' <- share' b
  t' <- share' t
  e' <- share' e
  return $ Cond b' t' e'
share' (Tap e t) = do
  e' <- share' e
  t' <- share' t
  return $ Tap e' t'
share' x = error $ "share': " ++ show x
