module BinOp
( union
, unions
, intersection
, intersections
, difference
, smoothUnion
, smoothUnions
, interp
, binopper
, allBinOps ) where

import E
import Lib

union :: BinOp
union = binopper union'
union' :: E Float -> E Float -> E Float
union' a b = smin a b

intersection :: BinOp
intersection = binopper intersection'
intersection' :: E Float -> E Float -> E Float
intersection' a b = smax a b

difference :: BinOp
difference = binopper difference'
difference' :: E Float -> E Float -> E Float
difference' a b = smax a (- b)

listify :: BinOp -> ([Shape] -> Shape)
-- TODO maybe default everything/nothing values for these?
listify op [] = error $ "empty listify"
listify op [x] = x
listify op [x, y] = x `op` y
listify op (x:ys) = x `op` (listify op ys)

unions :: [Shape] ->  Shape
unions = listify union

smoothUnions :: [Shape] ->  Shape
smoothUnions = listify smoothUnion

intersections :: [Shape] ->  Shape
intersections = listify intersection

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

interp :: E Float -> BinOp
interp alpha = binopper (interp' alpha)
interp' :: E Float -> E Float -> E Float -> E Float
interp' alpha a b = (1.0 - alpha) * a + alpha * b

allBinOps :: [BinOp]
allBinOps = [
    union
  , intersection
  , difference
  , smoothUnion
  ]
