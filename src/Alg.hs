module Alg
( Shp(..)
, ShpUnOp
, ShpBinOp
, shpEval ) where

import BinOp
import E
import Grid
import Prim
import Transform
import Util

-- TODO: rename Shp

type ShpUnOp = Shp -> Shp
type ShpBinOp = Shp -> Shp -> Shp

data Shp =
  -- prims
    Circle
  | Square
  | Flower (E Float)
  -- unops
  | Scale (E Float) Shp
  | Translation (E (V2 Float)) Shp
  | Rotation (E Float) Shp
  | Grid (E Float) (E Float) Shp
  | PfGrid (E Float) (E Float) Shp
  -- binops
  | Union Shp Shp
  | Intersection Shp Shp
  | Difference Shp Shp
  | SmoothUnion Shp Shp
  | Interp (E Float) Shp Shp
  deriving Show

shpEval :: Shp -> Shape
shpEval Circle = circle
shpEval Square = square
shpEval (Flower n) = flower n
shpEval (Scale e s) = scale e (shpEval s)
shpEval (Translation v s) = translation v (shpEval s)
shpEval (Rotation e s) = rotation e (shpEval s)
shpEval (Grid x y s) = grid x y (shpEval s)
shpEval (PfGrid x y s) = pfGrid x y (shpEval s)
shpEval (Union a b) = union (shpEval a) (shpEval b)
shpEval (Intersection a b) = intersection (shpEval a) (shpEval b)
shpEval (Difference a b) = difference (shpEval a) (shpEval b)
shpEval (SmoothUnion a b) = smoothUnion (shpEval a) (shpEval b)
shpEval (Interp e a b) = interp e (shpEval a) (shpEval b)
