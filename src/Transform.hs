module Transform
( speed
, scale
, translation
, rotation
, rotation'
, transform
, evalShape ) where

import E
import Lib

speed :: E -> UnOp
speed r = transform (speed' r)
  where speed' r (Transform xy t) = Transform xy (t * r)

rotMat :: E -> E
rotMat ang =
  let c = sh $ scos ang
      s = sh $ ssin ang
      mat = Mat2 [c, s, -s, c]
   in mat

scale :: E -> UnOp
scale s = transform (scale' s)

scale' :: E -> Transformer
scale' s (Transform xy t) = Transform (xy / s) t

translation :: E -> UnOp
translation dxy = transform (translation' dxy)

translation' :: E -> Transformer
translation' dxy (Transform xy t) = Transform (xy - dxy) t

rotation :: E -> UnOp
rotation ang = transform (rotation' ang)

rotation' :: E -> Transformer
rotation' ang (Transform xy t) =
  let c = sh $ scos ang
      s = sh $ ssin ang
      mat = sh $ Mat2 [c, s, -s, c]
   in Transform (mat * xy) t

transform :: Transformer -> Shape -> Shape
transform transformer p = p . transformer

idTransform :: Transform
idTransform = Transform XY time

evalShape :: Shape -> E
evalShape p = p idTransform
