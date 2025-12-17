module DropShadows
( dropShadow ) where

import BinOp
import Color
import E
import Transform
import Util hiding (die, time)

-- What's a Transform -> Color called?
dropShadow :: E (V2 Float) -> Color -> Color -> Color -> Shape -> (Transform -> Color)
dropShadow dir fg bg shadow shape transform =
  let shadowShape :: Shape
      shadowShape = translation dir shape
      dist = evalShape shape
      shadowDist = evalShape shadowShape
      bgCombined :: Color
      bgCombined = smooth shadow bg shadowDist
      all :: Color
      all = smooth fg bgCombined dist
   in all

-- smooth :: Color -> Color -> E Float -> Color
-- smooth fg bg dist =
