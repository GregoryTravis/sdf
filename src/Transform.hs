module Transform
( scale
, translation
, rotation
, rotation'
, transform
, evalShape ) where

import E
import Lib

rotMat :: E -> E
rotMat ang =
  let c = Sh $ scos ang
      s = Sh $ ssin ang
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
  let c = Sh $ scos ang
      s = Sh $ ssin ang
      mat = Sh $ Mat2 [c, s, -s, c]
   in Transform (mat * xy) t

transform :: Transformer -> Shape -> Shape
transform transformer p = p . transformer

idTransform :: Transform
idTransform = Transform XY time

evalShape :: Shape -> E
evalShape p = p idTransform
