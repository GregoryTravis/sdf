module Share
( share
, Refs ) where

import Control.Monad.State
import qualified Data.Map as M
import E

type Refs = M.Map Int E
type RevRefs = M.Map E Int
type RefState = (Int, Refs, RevRefs)
type Sharey a = State RefState a

initState :: RefState
initState = (0, M.empty, M.empty)

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
share' (Fun name tins tout es) = do
  es' <- mapM share' es
  return $ Fun name tins tout es'
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
share' (RGB e) = do
  e' <- share' e
  return $ RGB e'
share' (A e) = do
  e' <- share' e
  return $ A e'
share' x = return x
