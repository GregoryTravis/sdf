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

spiral :: Shape -> E Float -> Shape
spiral shape timeDelta =
  let t = time + timeDelta
      c = scale 0.1 shape
      yep = rotation t (translation (V2 (t / 7.0) 0) c)
   in yep

seqSpiral :: Shape
seqSpiral =
  let orig = spiral circle (KF 0.0)
      later = spiral square (KF (-1.0))
   in union orig later
