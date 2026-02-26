module Tap
( tap
, tapWith ) where

import Color
import E
import Lib
import Transform
import Util hiding (die, time)

tapWith :: GlslType a => E a -> (E a -> Color) -> E a
tapWith e f = Tap e (f e)

-- taps :: (Transform -> E Float) -> (Transform -> E Float)
tap :: Shape -> Shape
tap shp tr =
  -- use tapWith
  Tap (shp tr) (beColor $ evalShape shp)
    where beColor e = smooth white black e
