{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M

import Util

data Uniform = Uniform String
  deriving (Eq, Show, Read, Ord)

data E = KF Double | U Uniform | Add E E | Sub E E | Mul E E | Div E E | Length E | V2 E E | XY | Sh E | ShRef Int
  deriving (Eq, Show, Read, Ord)

type Refs = M.Map Int E
type RevRefs = M.Map E Int
type RefState = (Int, Refs, RevRefs)
type Sharey a = State RefState a

initState :: RefState
initState = (0, M.empty, M.empty)

share :: E -> (E, Refs)
share e =
  let (e', (_, refs, revRefs)) = runState (share' e) initState
   in eesp revRefs (e', refs)

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
share' x = return x

circle =
  let yeah = Sh $ U (Uniform "yeah")
      center = Sh $ V2 (Add (KF 2.0) yeah) (KF 2.0)
      radius = Sh $ KF 2.0
      cdist = Sub (Div (Length (Sub XY center)) radius) (KF 1.0)
   in cdist

-- // circle
-- vec2 center = vec2(0.2 + yeah, 0.2);
-- float radius = 0.2;
-- float cdist = (length(uv-center) / radius) - 1.0;

subexp :: Int -> String
subexp n = "x" ++ show n

parens :: String -> String
parens x = concat ["(", x, ")"]

op :: String -> String -> String -> String
op operator a b = concat [parens a, operator, parens b]

fun :: String -> [String] -> String
fun f args = parens $ concat [f, parens arglist]
  where arglist = intercalate ", " args

-- data E = KF Double | U Uniform | Add E E | Sub E E | Mul E E | Div E E | Length E | V2 E E | XY | Sh E | ShRef Int
compileE :: E -> String
compileE (KF d) = parens $ show d
compileE (U (Uniform name)) = parens name
compileE (Add a b) = op "+" (compileE a) (compileE b)
compileE (Sub a b) = op "-" (compileE a) (compileE b)
compileE (Mul a b) = op "*" (compileE a) (compileE b)
compileE (Div a b) = op "/" (compileE a) (compileE b)
compileE (Length e) = fun "length" [compileE e]
compileE (V2 a b) = fun "vec2" [compileE a, compileE b]
compileE XY = "(uv)"
compileE e@(Sh _) = error $ "Can't compile Sh nodes: " ++ show e
compileE (ShRef n) = parens $ subexp n

compileBinding :: String -> E -> String
compileBinding var e = concat [var, " = ", compileE e]

compileBindings :: [(String, E)] -> String
compileBindings bindings = intercalate ";\n" cbs ++ "\n"
  where cbs = map (\(var, e) -> compileBinding var e) bindings

compileGroup :: (E, Refs) -> String -> String
compileGroup (top, refs) topName = compileBindings bindings
  where bindings = reverse $ (topName, top) : shares
        shares = map (\(n, e) -> (subexp n, e)) (M.toList refs)

main = do
  msp $ compileGroup (share circle) "topp"
  msp "hi"
