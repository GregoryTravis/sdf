{-# Language DeriveFunctor, ScopedTypeVariables, StandaloneDeriving, TypeApplications #-}

module Commander
( commanderMain
, Commander(..)
, Result(..)
, mapCvt
, appl
, nest
, formatLog
, via ) where

import Control.Applicative
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.Maybe
import Data.Typeable
import Text.Read (readMaybe)

import Util

data Result a = Result (Maybe a) [String] deriving Functor

deriving instance Show a => Show (Result a)

data Commander a = Commander ([String] -> Result a) deriving Functor

type Parser a = String -> Result a

p2c :: Parser a -> Commander a
p2c p = Commander r
  where r [s] = p s
        r ss = Result Nothing ["p2c: expected 1, got " ++ show (length ss) ++ "(\"" ++ intercalate ", " ss ++ "\")"]

parse :: forall a. (Typeable a, Read a) => Commander a
parse = p2c $ baseParser s
  where s = "parse " ++ show (typeOf (Proxy @a))

via :: (Typeable a, Typeable b, Read a) => (a -> b) -> Commander b
via f = p2c $ cvtParser s f
  where s = "via (" ++ show (typeOf f) ++ ")"

cvtParser :: Read a => String -> (a -> b) -> Parser b
cvtParser ty cvt s =
  let parsedMaybe = readMaybe s
      success = case parsedMaybe
                  of Just _ -> "success"
                     Nothing -> "failure"
   in Result (fmap cvt parsedMaybe) [success ++ ": " ++ ty ++ " \"" ++ s ++ "\""]

baseParser :: Read a => String -> Parser a
baseParser ty = cvtParser ty id

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

instance Applicative Commander where
  -- TODO remove "pur" string?
  -- pure a = error "undefined (just uncomment the commented-out definition)"
  pure a = Commander r
    where r [] = Result (Just a) ["pur"]
  (Commander ssab) <*> (Commander ssa) = Commander r
    where r [] = error "heck: empty"
          r (s:ss) = ($) <$> ssab ss <*> ssa [s]

-- TODO use Maybe properly here
instance Semigroup (Commander a) where
  Commander f <> Commander g = Commander r
    where r ss = f ss <|> g ss

lookupWithExplanation :: (Show k, Ord k) => k -> M.Map k a -> Result a
lookupWithExplanation k m =
  case M.lookup k m of
    Just a -> Result (Just a) ["found " ++ show k]
    Nothing -> Result Nothing ["did not find " ++ show k ++ " in " ++ (intercalate ", " (map show $ M.keys m))]

-- TODO use Maybe properly here
mapCvt :: Show b => M.Map String b -> Commander b
mapCvt m = p2c cvt
  where cvt s = lookupWithExplanation s m

nest :: M.Map String (Commander a) -> Commander a
nest m = Commander r
  where r (s:ss) = lookupWithExplanation s m >>= flip appl ss

formatLog :: Result a -> String
formatLog (Result _ ss) = intercalate ", " ss

appl :: Commander a -> [String] -> Result a
appl (Commander f) ss = f ss

-- Test code

ilala :: Int -> Double
ilala x = fromIntegral $ x + 1

flala :: Double -> Double
flala x = x + 100.0

-- applWithDefault :: Commander a -> a -> [String] -> a
-- applWithDefault commander def command = fromMaybe def (appl commander command)

iplus :: Int -> Int -> Int
iplus = (+)

fplus :: Double -> Double -> Double
fplus = (+)

aNest :: Commander Double
aNest = nest $ M.fromList
  [ ("ilala", ilala <$> parse)
  , ("flala", flala <$> parse)
  , ("fplus", fplus <$> parse <*> parse)
  , ("sub", mapCvt $ M.fromList [("a", 110), ("b", 220)])
  ]

commanderMain = do
  -- msp $ appl (heck (pur ilala) (converter read)) ["4"]
  -- msp $ appl (heck (heck (pur iplus) (converter read)) (converter read)) ["4", "5"]
  -- msp $ appl (ilala <$> converter read) ["30"]
  -- msp $ appl (iplus <$> converter read <*> converter read) ["30", "41"]
  -- msp $ appl (fplus <$> converter read <*> parse) ["30.0", "41"]
  -- msp $ appl (iplus <$> converter read <*> parse) ["30.0", "41"]
  -- let il = ilala <$> parse
  --     fl = flala <$> parse
  --     ifl = il <> fl
  -- msp $ appl il ["30"]
  -- msp $ appl fl ["30.0"]
  -- msp $ appl ifl ["30"]
  -- msp $ appl ifl ["30.0"]
  -- msp $ appl il ["30.0"]
  -- let mc = mapCvt $ M.fromList [("a", 1::Int), ("b", 2)]
  -- msp $ appl mc ["a"]

  -- works
  -- msp $ appl aNest ["ilala", "122"]
  -- msp $ appl aNest ["flala", "122.0"]
  -- msp $ appl aNest ["fplus", "1000.0", "300.0"]
  -- msp $ appl aNest ["sub", "a"]
  msp "hi commander"
