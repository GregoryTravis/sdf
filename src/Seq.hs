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

-- Doesn't work because nothing is using the transformed time, only the global one.
-- The second cycle is slower because otherwise it would seem to line up properly with the first one just because both cycles take the same amount of time.
twoCycles :: Shape -- Transform -> Dist
twoCycles =
  let a = translation (V2 0.0 (-0.5)) (rotation time (translation (V2 0.0 0.5) (scale 0.5 circle)))
      b = rotation (kpi / (-4.0)) (translation (V2 0.0 (-0.5)) (rotation (time / KF 2.0) (translation (V2 0.0 0.5) (scale 0.5 circle))))
      oneCycleDuration = 2 * pi
   in sequ a oneCycleDuration b

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
