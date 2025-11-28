module Transform
( scale
, translation
, rotation
, rotation'
, transform
, evalShape ) where

import E
import Lib

-- speed :: E -> UnOp
-- speed r = transform (speed' r)
--   where speed' r (Transform xy t) = Transform xy (t * r)

-- rotMat :: E -> E
-- rotMat ang =
--   let c = sh $ scos ang
--       s = sh $ ssin ang
--       mat = Mat2 [c, s, -s, c]
--    in mat

scale :: E Float -> UnOp
scale s = transform (scale' s)

scale' :: E Float -> Transformer
scale' s (Transform xy t) = Transform (xy /^ s) t

translation :: E (V2 Float) -> UnOp
translation dxy = transform (translation' dxy)

translation' :: E (V2 Float) -> Transformer
translation' dxy (Transform xy t) = Transform (xy -^ dxy) t

rotation :: E Float -> UnOp
rotation ang = transform (rotation' ang)

rotation' :: E Float -> Transformer
rotation' ang (Transform xy t) =
  let c = sh $ scos ang
      s = sh $ ssin ang
      mat = sh $ Mat2 [c, s, -s, c]
   in Transform (mat *^ xy) t

transform :: Transformer -> Shape -> Shape
transform transformer p = p . transformer

-- This time parameter is not used by any temporal functions, which just
-- reference the uniform 'time' directly, so they have no effect. All prims
-- ignore it.
-- timeShift :: E Float -> UnOp
-- timeShift dt = transform (timeShift' dt)
-- timeShift' :: E Float -> Transformer
-- timeShift' dt (Transform xy t) = Transform xy (t + dt)

idTransform :: Transform
idTransform = Transform uv time

evalShape :: Shape -> Dist
evalShape p = p idTransform
