{-# Language DeriveFunctor, StandaloneDeriving #-}

module Commander
( commanderMain
, Commander(..)
, Result(..)
, mapCvt
, appl
-- , applWithDefault
, nest
-- , rm
, formatLog
, p2c
, cvtParser ) where

import Control.Applicative
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.Maybe
import Text.Read (readMaybe)

import Util

data Result a = Result (Maybe a) [String] deriving Functor

deriving instance Show a => Show (Result a)

formatLog :: Result a -> String
formatLog (Result _ ss) = intercalate ", " ss

data Commander a = Commander ([String] -> Result a) deriving Functor

type Parser a = String -> Result a

p2c :: Parser a -> Commander a
p2c p = Commander r
  where r [s] = p s
        r _ = error "p2c"

cvtParser :: Read a => String -> (a -> b) -> Parser b
-- cvtParser ty cvt s = Result (fmap cvt $ readMaybe s) ["base " ++ ty ++ " \"" ++ s ++ "\""]
cvtParser ty cvt s =
  let parsedMaybe = readMaybe s
      success = case parsedMaybe
                  of Just _ -> "success"
                     Nothing -> "failure"
   in Result (fmap cvt parsedMaybe) [success ++ " " ++ ty ++ " \"" ++ s ++ "\""]

baseParser :: Read a => String -> Parser a
baseParser ty = cvtParser ty id

-- TODO use TypeRep to get the type automatically
floatParser :: Parser Float
floatParser = baseParser "Float"

rm :: Read a => Commander a
rm = p2c $ baseParser "rm"

-- TODO use Maybe properly here
heck :: Commander (a -> b) -> Commander a -> Commander b
heck (Commander ssab) (Commander ssa) = Commander r
  where r [] = error "heck: empty"
        r ss = ($) <$> ssab (init ss) <*> ssa [last ss]

-- TODO remove "pur" string?
pur :: a -> Commander a
pur a = Commander r
  where r [] = Result (Just a) ["pur"]

-- TODO remove "pure" string?
instance Applicative Result where
  pure a = Result (Just a) ["pure"]
  Result ma ssa <*> Result mb ssb = Result (ma <*> mb) (ssa ++ ssb)

instance Alternative Result where
  empty = Result Nothing []
  Result ma ssa <|> Result mb ssb = Result (ma <|> mb) (ssa ++ ssb)

instance Monad Result where
  Result (Just a) ssa >>= k =
    case k a of Result mb ssb -> Result mb (ssa ++ ssb)
  Result Nothing ssa >>= _ = Result Nothing ssa
  -- Result ma ssa >>= k =
  --   case ma of
  --     Just a -> case k a of Result mb ssb -> Result mb (ssa ++ ssb)
  --     Nothing -> Result ma nothing

instance Applicative Commander where
  pure = pur
  (<*>) = heck

-- TODO use Maybe properly here
instance Semigroup (Commander a) where
  Commander f <> Commander g = Commander r
    where r ss = f ss <|> g ss

-- converter :: (String -> a) -> Commander a
-- converter c = Commander r
--   where r [s] = Just (c s)

-- converterMaybe :: (String -> Result a) -> Commander a
-- converterMaybe c = Commander r
--   where r [s] = c s

-- converterWith :: Read a => (a -> b) -> (???) -> Commander b

-- rm :: Read a => Commander a
-- rm = p2c readMaybe

lookupWithExplanation :: (Show k, Ord k) => k -> M.Map k a -> Result a
lookupWithExplanation k m =
  case M.lookup k m of
    Just a -> Result (Just a) ["found " ++ show k]
    Nothing -> Result Nothing ["did not find " ++ show k ++ " in " ++ (intercalate ", " (map show $ M.keys m))]

-- TODO use Maybe properly here
mapCvt :: Show b => M.Map String b -> Commander b
mapCvt m = p2c cvt
  where cvt s = eeesp ("mapCVT", m, s) $ lookupWithExplanation s m

-- TODO use Maybe properly here
-- mapCvtR :: (Read a, Ord a) => M.Map a b -> Commander b
-- mapCvtR m = p2c cvt
--   where cvt s = readMaybe s >>= flip lookupWithExplanation m

nest :: M.Map String (Commander a) -> Commander a
nest m = Commander r
  where r (s:ss) = lookupWithExplanation s m >>= flip appl ss

ilala :: Int -> Double
ilala x = fromIntegral $ x + 1

flala :: Double -> Double
flala x = x + 100.0

appl :: Commander a -> [String] -> Result a
appl (Commander f) ss = f ss

-- applWithDefault :: Commander a -> a -> [String] -> a
-- applWithDefault commander def command = fromMaybe def (appl commander command)

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
