{-# Language DeriveFunctor  #-}

module Commander
( commanderMain
, Commander(..)
, mapCvt
, appl
, nest
, rm
, converterMaybe ) where

import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Read (readMaybe)

import Util

data Commander a = Commander ([String] -> Maybe a) deriving Functor

-- TODO use Maybe properly here
heck :: Commander (a -> b) -> Commander a -> Commander b
heck (Commander ssab) (Commander ssa) = Commander r
  where r (s:ss) =
          let mf = ssab ss
              ma = ssa [s]
           in case (mf, ma)
                of (Just f, Just a) -> Just (f a)
                   _ -> Nothing

pur :: a -> Commander a
pur a = Commander r
  where r [] = Just a

instance Applicative Commander where
  pure = pur
  (<*>) = heck

-- TODO use Maybe properly here
instance Semigroup (Commander a) where
  Commander f <> Commander g = Commander r
    where r ss = case f ss
                   of Just x -> Just x
                      Nothing -> g ss

converter :: (String -> a) -> Commander a
converter c = Commander r
  where r [s] = Just (c s)

converterMaybe :: (String -> Maybe a) -> Commander a
converterMaybe c = Commander r
  where r [s] = c s

-- converterWith :: Read a => (a -> b) -> (???) -> Commander b

rm :: Read a => Commander a
rm = converterMaybe readMaybe

-- TODO use Maybe properly here
mapCvt :: Show b => M.Map String b -> Commander b
mapCvt m = converterMaybe cvt
  where cvt s = eeesp ("mapCVT", m, s) $ M.lookup s m

-- TODO use Maybe properly here
mapCvtR :: (Read a, Ord a) => M.Map a b -> Commander b
mapCvtR m = converterMaybe cvt
  where cvt s =
          case readMaybe s
            of Just s -> M.lookup s m
               Nothing -> Nothing

nest :: M.Map String (Commander a) -> Commander a
nest m = Commander r
  where r (s:ss) =
          case eesp ("nest lookup", s, M.keys m, s == head (M.keys m), isNothing $ M.lookup s m) $ M.lookup s m
            of Just c -> eesp ("nest appl") $ appl c ss
               Nothing -> Nothing

ilala :: Int -> Double
ilala x = fromIntegral $ x + 1

flala :: Double -> Double
flala x = x + 100.0

appl :: Commander a -> [String] -> Maybe a
appl (Commander f) ss = f ss

iplus :: Int -> Int -> Int
iplus = (+)

fplus :: Double -> Double -> Double
fplus = (+)

aNest :: Commander Double
aNest = nest $ M.fromList
  [ ("ilala", ilala <$> rm)
  , ("flala", flala <$> rm)
  , ("fplus", fplus <$> rm <*> rm)
  , ("sub", mapCvt $ M.fromList [("a", 110), ("b", 220)])
  ]

commanderMain = do
  -- msp $ appl (heck (pur ilala) (converter read)) ["4"]
  -- msp $ appl (heck (heck (pur iplus) (converter read)) (converter read)) ["4", "5"]
  -- msp $ appl (ilala <$> converter read) ["30"]
  -- msp $ appl (iplus <$> converter read <*> converter read) ["30", "41"]
  -- msp $ appl (fplus <$> converter read <*> rm) ["30.0", "41"]
  -- msp $ appl (iplus <$> converter read <*> rm) ["30.0", "41"]
  -- let il = ilala <$> rm
  --     fl = flala <$> rm
  --     ifl = il <> fl
  -- msp $ appl il ["30"]
  -- msp $ appl fl ["30.0"]
  -- msp $ appl ifl ["30"]
  -- msp $ appl ifl ["30.0"]
  -- msp $ appl il ["30.0"]
  -- let mc = mapCvt $ M.fromList [("a", 1::Int), ("b", 2)]
  -- msp $ appl mc ["a"]
  msp $ appl aNest ["ilala", "122"]
  msp $ appl aNest ["flala", "122.0"]
  msp $ appl aNest ["fplus", "1000.0", "300.0"]
  msp $ appl aNest ["sub", "a"]
  msp "hi commander"
