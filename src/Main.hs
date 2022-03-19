module Main where

import Control.Monad.State
import qualified Data.Map as M

import Util

-- // circle
-- vec2 center = vec2(0.2 + yeah, 0.2);
-- float radius = 0.2;
-- float cdist = (length(uv-center) / radius) - 1.0;

data Uniform = UF String
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
  let yeah = Sh $ U (UF "yeah")
      center = Sh $ V2 (Add (KF 2.0) yeah) (KF 2.0)
      radius = Sh $ KF 2.0
      cdist = Sub (Div (Length (Sub XY center)) radius) (KF 1.0)
   in cdist

sharing =
  let a = Sh XY
      b = Sh $ Add a a
   in Mul b b

main = do
  msp circle
  msp $ share circle
  msp "hi"
