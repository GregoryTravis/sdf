{-# LANGUAGE GADTs, RankNTypes, StandaloneDeriving  #-}

module Compile
(   compileBinding
  , compileFunction
  , compileSingle
) where

import Control.Monad (unless)
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as M
import System.Mem.StableName

import E
import Transform
import Util

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
compileSubE e@(Share _ _) = error $ "Can't compile Share nodes: " ++ show e
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
deriving instance Show DSN

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
  let (e', (_, semap)) = runState (share'' e) initState
   in (e', semap)

-- -- Assign a unique small integer to each sn. Maybe I could use hashStableName
-- -- but it's not clear how unlikely that is to have a collision
-- toRefs :: CapMap -> RefMap -> Refs
-- toRefs capMap refMap =
--   let sns = HM.keys capMap
--       refs = Prelude.map (\sn -> refMap HM.! sn) sns
--       caps = Prelude.map (\sn -> capMap HM.! sn) sns
--    in M.fromList (zip refs caps)

-- TODO possibly this is a tad slower than the non-generic one?
xform :: forall a. (Show a, GlslType a) =>
  (forall a. (Show a, GlslType a) => E a -> State ShareState (E a)) ->
  (forall a. (Show a, GlslType a) => E a -> State ShareState (E a)) ->
  (E a -> State ShareState (E a))
xform before after e = do
  -- TODO some >>= thing here
  e' <- before e
  e'' <- descend e'
  e''' <- after e''
  return e'''
  where
    rec :: forall a. (Show a, GlslType a) => (E a -> State ShareState (E a))
    rec = xform before after
    descend :: (Show a, GlslType a) => E a -> State ShareState (E a)
    descend (Share sn e) = do
      e' <- rec e
      return $ Share sn e'
    descend e@(ShareRef n) = return e
    descend e@(KF _) = return e
    descend e@(KD _) = return e
    descend e@(KI _) = return e
    descend (V2 a b) = do
      a' <- rec a
      b' <- rec b
      return $ V2 a' b'
    descend (V3 a b c) = do
      a' <- rec a
      b' <- rec b
      c' <- rec c
      return $ V3 a' b' c'
    descend (V4 a b c d) = do
      a' <- rec a
      b' <- rec b
      c' <- rec c
      d' <- rec d
      return $ V4 a' b' c' d'
    descend (Add a b) = do
      a' <- rec a
      b' <- rec b
      return $ Add a' b'
    descend (Sub a b) = do
      a' <- rec a
      b' <- rec b
      return $ Sub a' b'
    descend (Mul a b) = do
      a' <- rec a
      b' <- rec b
      return $ Mul a' b'
    descend (Div a b) = do
      a' <- rec a
      b' <- rec b
      return $ Div a' b'
    descend (And a b) = do
      a' <- rec a
      b' <- rec b
      return $ And a' b'
    descend (Or a b) = do
      a' <- rec a
      b' <- rec b
      return $ Or a' b'
    descend (Length e) = do
      e' <- rec e
      return $ Length e'
    descend e@(Uniform _) = return e
    descend (Swizzle swizzler e) = do
      e' <- rec e
      return $ Swizzle swizzler e'
    descend (Field fielder e) = do
      e' <- rec e
      return $ Field fielder e'
    descend (Fun1 name a) = do
      a' <- rec a
      return $ Fun1 name a'
    descend (Fun2 name a b) = do
      a' <- rec a
      b' <- rec b
      return $ Fun2 name a' b'
    descend (Fun3 name a b c) = do
      a' <- rec a
      b' <- rec b
      c' <- rec c
      return $ Fun3 name a' b' c'
    descend (Method0 name a) = do
      a' <- rec a
      return $ Method0 name a'
    descend (Neg e) = do
      e' <- rec e
      return $ Neg e'
    descend (Mat2 es) = do
      es' <- mapM rec es
      return $ Mat2 es'
    descend (Arr es) = do
      es' <- mapM rec es
      return $ Arr es'
    descend (ArrLookup arr index) = do
      arr' <- rec arr
      index' <- rec index
      return $ ArrLookup arr' index'
    descend (Comparison op a b) = do
      a' <- rec a
      b' <- rec b
      return $ Comparison op a' b'
    descend (Cond b t e) = do
      b' <- rec b
      t' <- rec t
      e' <- rec e
      return $ Cond b' t' e'
    descend (Tap e t) = do
      e' <- rec e
      t' <- rec t
      return $ Tap e' t'
    descend x = error $ "rec: " ++ show x

-- Allocate a fresh tag n for expression e, add (n -> e') to the map state,
-- where e' is the result of the recursive call to share' on e.
-- Return (ShRef n).
share'' :: (Show a, GlslType a) => (E a -> State ShareState (E a))
share'' = xform share'Before share'After

share'Before :: (Show a, GlslType a) => E a -> State ShareState (E a)
share'Before x@(Share sn e) = do
  -- In this case we only recurse into `e` if we haven't already seen this node.
  hc <- hasSE sn
  if hc
    then do n <- getRef sn
            return $ ShareRef n
    else return x
share'Before e = return e

share'After :: (Show a, GlslType a) => E a -> State ShareState (E a)
share'After (Share sn e) = do
  hc <- hasSE sn
  unless hc $ do
    addSE sn e
  n <- getRef sn
  return $ ShareRef n
share'After e = return e
