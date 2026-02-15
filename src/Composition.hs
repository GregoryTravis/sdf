module Composition
( composeCircular ) where

import E
import Color
import Prim
import Util hiding (time)


-- composeCircular :: (Transform -> E Float) -> (Transform -> E Float) -> (Transform -> E Float)
composeCircular :: Shape -> Shape -> Shape
composeCircular s0 s1 tr =
  let d0 = s0 tr + 1.0 // -1 -> 0
      d0 :: E Float
      (gx, gy) = (sdFdx d0, sdFdy d0)
      (gx, gy) :: (E Float, E Float)
      theta = satan gy gx
      ux = cos theta
      uy = sin theta
      x = d0 * ux
      y = d0 * uy
      ctr = Transform (V2 x y) time
      d1 = s1 ctr
   in d1
