module Seq where

import Alg
import BinOp
import Bsp
import Color
import Commander
import Composition
import DropShadows
import E
import Funs
import Grid
import Lib
import Prim
import Tap
import Transform
import Util hiding (die, time)

sequ :: Shape -> Float -> Shape -> Shape
sequ a dur b tr@(Transform xy t) =
  let kDur = KF dur
   in Cond (t <. kDur)
           (a tr)
           (b (Transform xy (t - kDur)))

circleOutline = circle `difference` (scale 0.9) circle
squareOutline = square `difference` (scale 0.9) square

spiral :: UnOp a
spiral =
   (tRotation id) . (tTranslation $ \t -> V2 (t / 7.0) 0)

--timeDelay :: E Float -> (Transform -> a) -> (Transform -> a)
timeDelay :: E Float -> UnOp a
timeDelay dt shape (Transform xy t) = shape (Transform xy (t + dt))

seqSpiral :: Shape
seqSpiral =
  let c = scale 0.05 circleOutline
      s = scale 0.05 squareOutline
      orig = spiral c
      later = timeDelay (-1.0) $ spiral s
      both = union orig later
   in union (scale 0.05 circle) both

seqTwoSteps :: Shape
seqTwoSteps =
  let c = scale 0.05 circleOutline
      or = scale 0.05 squareOutline
      one = tRotation id $ translation (V2 (-0.2) 0) c
      two = translation (V2 0.4 0) $ tRotation negate $ translation (V2 (-0.2) 0) c
   in or `union` (sequ one 3.141592653589793 two)
