module Bsp
( bsp
, bspColor
) where

import Color
import E
import Transform
import Util

-- This of course works with any shape, not just a half-space. But later code
-- might well assume it's a half-space.
bsp :: Shape -> Shape -> Shape -> Shape
bsp splitter neg pos tr =
  let splitDist = splitter tr
   in (Cond (splitDist <. 0) (neg tr) (pos tr))

bspColor :: (Transformable Dist) -> (Transformable Color) -> (Transformable Color) -> (Transformable Color)
bspColor splitter neg pos tr =
  let splitDist = splitter tr
   in (Cond (splitDist <. 0) (neg tr) (pos tr))
