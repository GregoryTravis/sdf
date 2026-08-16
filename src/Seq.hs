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

spiral :: Shape -> E Float -> Shape
spiral shape timeDelta =
  let t = time + timeDelta
      c = scale 0.05 shape
      yep = rotation t (translation (V2 (t / 7.0) 0) c)
   in yep

nspiral :: Shape -> E Float -> Shape
nspiral shape timeDelta tr@(Transform xy tt) =
  let t = tt + timeDelta
      c = scale 0.05 shape
      yep = rotation t (translation (V2 (t / 7.0) 0) c)
   in yep tr

timeDelay :: E Float -> (Transform -> a) -> (Transform -> a)
timeDelay dt shape (Transform xy t) = shape (Transform xy (t + dt))

seqSpiral :: Shape
seqSpiral =
  let c = circleOutline
      s = squareOutline
      orig = nspiral c (KF 0.0)
      later = nspiral s (KF (-1.0))
      later2 = timeDelay (-1.0) $ nspiral s (KF 0.0)
      both = union orig later2
   in union (scale 0.05 circle) both
