module BinOp
( union
, intersection
, difference
, smoothUnion
, interp
, binopper
, allBinOps ) where

import E
import Lib

union :: BinOp
union = binopper union'
union' :: E -> E -> E
union' a b = Min a b

intersection :: BinOp
intersection = binopper intersection'
intersection' :: E -> E -> E
intersection' a b = Max a b

difference :: BinOp
difference = binopper difference'
difference' :: E -> E -> E
difference' a b = Max a (- b)

binopper :: (E Float -> E Float -> E Float) -> BinOp
binopper distCombiner p0 p1 tr = distCombiner (sh $ p0 tr) (sh $ p1 tr)

smoothUnion :: BinOp
smoothUnion = binopper smoothUnion'

smoothUnion' :: E Float -> E Float -> E Float
smoothUnion' usd0 usd1 =
  let d0 = sh usd0
      d1 = sh usd1
      r = KF 0.3
      md0 = sh $ smin (d0 -^ r) (KF 0.0)
      md1 = sh $ smin (d1 -^ r) (KF 0.0)
      inside_distance = sh $ - (ssqrt $ (md0 *^ md0) +^ (md1 *^ md1))
      simple_union = sh $ smin d0 d1
      outside_distance = sh $ smax simple_union r
      dist = sh $ inside_distance + outside_distance
   in dist

interp :: E -> BinOp
interp alpha = binopper (interp' alpha)
interp' :: E -> E -> E -> E
interp' alpha a b = (1.0 - alpha) * a + alpha * b

allBinOps :: [BinOp]
allBinOps = [
    union
  , intersection
  , difference
  , smoothUnion
  ]
