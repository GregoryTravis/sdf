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
  | TScale (E Float -> E Float) Shp
  | TTranslation (E Float -> E (V2 Float)) Shp
  | TRotation (E Float -> E Float) Shp
  | Grid (E Float) (E Float) Shp
  | PfGrid (E Float) (E Float) Shp
  | TPfGrid (E Float -> E Float) (E Float -> E Float) Shp
  -- binops
  | Union Shp Shp
  | Intersection Shp Shp
  | Difference Shp Shp
  | SmoothUnion Shp Shp
  | Interp (E Float) Shp Shp
  --deriving Show -- impossible now because f the T* ctors that take a function

shpEval :: Shp -> Shape
shpEval Circle = circle
shpEval Square = square
shpEval (Flower n) = flower n
shpEval (Scale e s) = scale e (shpEval s)
shpEval (Translation f s) = translation f (shpEval s)
shpEval (Rotation e s) = rotation e (shpEval s)
shpEval (TScale ef s) = tScale ef (shpEval s)
shpEval (TTranslation vf s) = tTranslation vf (shpEval s)
shpEval (TRotation ef s) = tRotation ef (shpEval s)
shpEval (Grid x y s) = grid x y (shpEval s)
shpEval (PfGrid x y s) = pfGrid x y (shpEval s)
shpEval (TPfGrid xf yf s) = tPFGrid xf yf (shpEval s)
shpEval (Union a b) = union (shpEval a) (shpEval b)
shpEval (Intersection a b) = intersection (shpEval a) (shpEval b)
shpEval (Difference a b) = difference (shpEval a) (shpEval b)
shpEval (SmoothUnion a b) = smoothUnion (shpEval a) (shpEval b)
shpEval (Interp e a b) = interp e (shpEval a) (shpEval b)
