module Interactive
( mouseCircleE ) where

import E
import Color
import Lib
import Transform
import Util

mouseCircle :: Shape
mouseCircle (Transform xy _) =
  let dist = Length xy - r
      r = Length mouse
   in dist

mouseCircleE :: IO (E (V4 Float))
mouseCircleE = return $ smooth white black $ evalShape mouseCircle
