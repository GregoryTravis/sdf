{-# LANGUAGE GADTs #-}

module Share
( share ) where

import Control.Monad.State
import Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Map.Strict as M
import System.Mem.StableName

import E

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
