{-# LANGUAGE DeriveGeneric, EmptyDataDeriving, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, StandaloneDeriving #-}

module Typed
( typedMain ) where

import Util

-- Use DataKinds?
data V2 a
  deriving Show

data E a where
  KF :: Float -> E Float
  KI :: Int -> E Int
  V2 :: Show a => E a -> E a -> E (V2 a)
  Add :: Addable a b c => E a -> E b -> E c

deriving instance Show a => Show (E a)

class (Show a, Show b, Show c) => Addable a b c | a b -> c where
instance Addable Int Int Int
instance Addable Int Float Float
-- Could there be an abbreviation, like this?
--   class SelfAddable a => Addable a a a
instance Addable (V2 Float) (V2 Float) (V2 Float)
instance Addable (V2 Float) Float (V2 Float)

-- blah :: E Int
blahii = Add (KI 1) (KI 2)
blahif = Add (KI 1) (KF 2.0)
blahvv = Add (V2 (KF 1.0) (KF 1.0)) (V2 (KF 1.0) (KF 1.0))
blahvf = Add (V2 (KF 1.0) (KF 1.0)) (KF 2.0)

-- Experimenting with a type paramter for E.
typedMain = do
  msp blahii
  msp blahif
  msp blahvv
  msp blahvf
  msp "typed hi"
