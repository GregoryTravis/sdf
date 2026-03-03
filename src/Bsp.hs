module Bsp
( bsp
) where

import Color
import E
import Transform
import Util

-- This of course works with any shape, not just a half-space. But later code
-- might well assume it's a half-space.
bsp :: (Show a, GlslType a) => Shape -> (Transformable (E a)) -> (Transformable (E a)) -> (Transformable (E a))
bsp splitter neg pos tr =
  let splitDist = splitter tr
   in (Cond (splitDist <. 0) (neg tr) (pos tr))
