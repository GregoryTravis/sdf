module Bsp
( bsp
, bspColor
) where

import Color
import E
import Lib
import Transform
import Util

-- This of course works with any shape, not just a half-space. But later code
-- might well assume it's a half-space.
bsp :: Shape -> (Transformable Dist) -> (Transformable Dist) -> (Transformable Dist)
bsp splitter neg pos tr =
  let splitDist = splitter tr
   in smoothDist (pos tr) (neg tr) splitDist

bspColor :: (Transformable Dist) -> (Transformable Color) -> (Transformable Color) -> (Transformable Color)
bspColor splitter neg pos tr =
  let splitDist = splitter tr
   in smooth (pos tr) (neg tr) splitDist

smoothDist :: Dist -> Dist -> Dist -> Dist
smoothDist fg bg dist =
  let smoothRadius = scaleAwareAA dist
      bwBlend = smoothstep (-smoothRadius) smoothRadius dist
      -- TODO use mix
      d = bwBlend *^ bg +^ (KF 1.0 -^ bwBlend) *^ fg;
   in sh d
