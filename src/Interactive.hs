module Interactive
( mouseCircleE ) where

import E
import Color
import Transform
import Util

mouseCircle :: Shape
mouseCircle (Transform xy _) =
  let dist = Length xy - r
      r = Length Mouse
   in dist

mouseCircleE :: IO E
mouseCircleE = return $ smooth white black $ evalShape mouseCircle
