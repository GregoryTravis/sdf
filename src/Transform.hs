{-# LANGUAGE TypeSynonymInstances #-}

module Transform
( scale
, translation
, rotation
, rotation'
, flipX
, flipY
, transform
, idTransform
, evalShape ) where

import E
import Lib

--instance Applicative ((->) Transform) where
--  pure = undefined
--  (<*>) = undefined
--  --pure x _transform = x
--  --(f <*> x) transform = (f transform) (x transform)

-- speed :: E -> UnOp
-- speed r = transform (speed' r)
--   where speed' r (Transform xy t) = Transform xy (t * r)

-- rotMat :: E -> E
-- rotMat ang =
--   let c = sh $ scos ang
--       s = sh $ ssin ang
--       mat = Mat2 [c, s, -s, c]
--    in mat

-- :: E Float -> (Transform -> Dist) -> (Transform -> Dist)
scale :: E Float -> UnOp a
scale s = transform (scale' s)

scale' :: E Float -> Transformer
scale' s (Transform xy t) = Transform (xy /^ s) t

translation :: E (V2 Float) -> UnOp a
translation dxy = transform (translation' dxy)

translation' :: E (V2 Float) -> Transformer
translation' dxy (Transform xy t) = Transform (xy -^ dxy) t

rotation :: E Float -> UnOp a
rotation ang = transform (rotation' ang)

rotation' :: E Float -> Transformer
rotation' ang (Transform xy t) =
  let c = sh $ scos ang
      s = sh $ ssin ang
      mat = sh $ Mat2 [c, s, -s, c]
   in Transform (mat *^ xy) t

flipX :: UnOp a
flipX s = transform (\(Transform xy t) -> (Transform (V2 (-(_x xy)) (_y xy)) t)) s

flipY :: UnOp a
flipY s = transform (\(Transform xy t) -> (Transform (V2 (_x xy) (-(_y xy))) t)) s

transform :: Transformer -> Transformable a -> Transformable a
transform transformer p = p . transformer

-- Generic scaler, not shape-specific, for modgrid rainbow
transform' :: Transformer -> Transformable a -> Transformable a
transform' transformer p = p . transformer

-- This time parameter is not used by any temporal functions, which just
-- reference the uniform 'time' directly, so they have no effect. All prims
-- ignore it.
-- timeShift :: E Float -> UnOp
-- timeShift dt = transform (timeShift' dt)
-- timeShift' :: E Float -> Transformer
-- timeShift' dt (Transform xy t) = Transform xy (t + dt)

idTransform :: Transform
idTransform = Transform uv time

evalShape :: Transformable a -> a
evalShape p = p idTransform
