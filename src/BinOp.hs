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

binopper :: (E -> E -> E) -> BinOp
binopper distCombiner p0 p1 tr = distCombiner (Sh $ p0 tr) (Sh $ p1 tr)

smoothUnion :: BinOp
smoothUnion = binopper smoothUnion'

smoothUnion' :: E -> E -> E
smoothUnion' usd0 usd1 =
  let d0 = Sh usd0
      d1 = Sh usd1
      r = 0.3
      md0 = Sh $ Min (d0 - r) 0.0
      md1 = Sh $ Min (d1 - r) 0.0
      inside_distance = - (ssqrt $ (md0 * md0) + (md1 * md1))
      simple_union = Min d0 d1
      outside_distance = Max simple_union r
      dist = inside_distance + outside_distance
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
