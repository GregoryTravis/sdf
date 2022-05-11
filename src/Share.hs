module Share
( share
, Refs ) where

import Control.Monad.State
import Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Map.Strict as M
import E

type CapMap = HM.HashMap StableName E
type RefMap = HM.HashMap StableName Int
type Refs = M.Map Int E

type ShareState = (Int, CapMap, RefMap)

initState :: RefState
initState = (0, HM.empty, HM.empty)

addCap :: E -> StableName -> State ShareState ()
addCap cap sn = do
  (n, capMap, refMap) <- get
  let n' = n + 1
      capMap' = HM.insert sn cap capMap
      refMap' = HM.insert sn n refMap
  put (n', capMap', refMap')

hasCap :: StableName -> State ShareState Bool
hasCap sn = do
  (_, capMap, _) <- get
  return $ HM.member sn capMap

getRef :: StableName -> State ShareState Int
getRef sn = do
  (_, _, refMap) <- get
  case HM.lookup sn refMap of
    Just n -> return n
    Nothing -> error "missing from refMap"

-- Extract Sh nodes from e and store them in the map.
share :: E -> (E, Refs)
share e =
  let (e', capMap, refMap) = runState (share' e) initState
      refs = toRefs capMap refMap
   in (e', refs)

-- Assign a unique small integer to each sn. Maybe I could use hashStableName
-- but it's not clear how unlikely that is to have a collision
toRefs :: CapMap -> RefMap -> Refs
toRefs capMap refMap =
  let sns = HM.keys capMap
      refs = map (\sn -> refMap HM.! sn)
      caps = map (\sn -> capMap HM.! sn)
   in HM.fromList (zip refs caps)

-- Allocate a fresh tag n for this node, add (n -> e) to the map state, and
-- return (ShRef n).
-- Also recursively call share on the contents of the Sh node.
share' :: E -> State ShareState E
share' (Share e sn) = do
  hc <- hasCap sn
  if hc
    then do n <- getRef sn
            return $ ShareRef n
    else do eCap <- share' e
            addCap eCap sn
            n <- getRef sn
            return $ ShareRef n

  -- capMap <- get
  -- case HM.lookup sn capMap of
  --   Nothing -> do
  --     e' <- share' e
  --     capMap' <- get
  --     put $ HM.insert sn e' capMap'
  --     return (ShareRef sn)
  --   Just e' -> do
  --     return (ShareRef sn)

  -- e' <- share' e
  -- (n, refs, revRefs) <- get
  -- case revRefs M.!? e' of
  --   Just n -> return $ ShRef n
  --   Nothing -> do
  --     let refs' = M.insert n e' refs
  --     let revRefs' = M.insert e' n revRefs
  --     put (n + 1, refs', revRefs')
  --     return $ ShRef n
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

instance Show (StableName a) where
  show sn = show (hashStableName sn)
