module Grid
( grid
, pfGrid
, modgrid
, modgrid' ) where

import E
import Lib
import Transform

grid :: E Float -> E Float -> UnOp
grid w h = transform (grid' w h)

grid' :: E Float -> E Float -> Transformer
grid' w h (Transform xy t) =
  let x = sh $ _x xy
      y = sh $ _y xy
      xx = sh $ smod x w
      yy = sh $ smod y h
      xi = sh $ sfloor x
      yi = sh $ sfloor y
   in Transform (V2 xx yy) t

-- TODO xi, yi should be E Int
modgrid :: E Float -> E Float -> (E Float -> E Float -> Shape) -> Shape
modgrid w h modShaper (Transform xy t) =
  let x = sh $ _x xy
      y = sh $ _y xy
      xx = sh $ smod x w
      yy = sh $ smod y h
      xi = sh $ sfloor x
      yi = sh $ sfloor y
      newt = Transform (V2 xx yy) t
   in (modShaper xi yi) newt

-- Generic scaler, not shape-specific, for modgrid rainbow
-- TODO xi, yi should be E Int
modgrid' :: E Float -> E Float -> (E Float -> E Float -> (Transform -> a)) -> (Transform -> a)
modgrid' w h modShaper (Transform xy t) =
  let x = sh $ _x xy
      y = sh $ _y xy
      xx = sh $ smod x w
      yy = sh $ smod y h
      xi = sh $ sfloor x
      yi = sh $ sfloor y
      newt = Transform (V2 xx yy) t
   in (modShaper xi yi) newt

bugPfGrid :: E Float -> E Float -> UnOp
bugPfGrid w h = transform (bugPfGrid' w h)

bugPfGrid' :: E Float -> E Float -> Transformer
bugPfGrid' w h (Transform xy t) =
  let x = _x xy
      y = _y xy
      xx = smod x w
      yy = smod y h
      xi = sfloor x
      yi = sfloor y
      xx2 = Cond (smod (sabs xi) 2 ==. 1) (w - xx) xx
      yy2 = Cond (smod (sabs yi) 2 ==. 1) (h - yy) yy
   in Transform (V2 xx2 yy2) t

bugPfGrid2 :: E Float -> E Float -> UnOp
bugPfGrid2 w h = transform (bugPfGrid2' w h)

bugPfGrid2' :: E Float -> E Float -> Transformer
bugPfGrid2' w h (Transform xy t) =
  let x = _x xy
      y = _y xy
      xx = smod x w
      yy = smod y h
      xi = sfloor x
      yi = sfloor y
      xx2 = Cond (smod xi 2 ==. 1) (w - xx) xx
      yy2 = Cond (smod yi 2 ==. 1) (h - yy) yy
   in Transform (V2 xx2 yy2) t

pfGrid :: E Float -> E Float -> UnOp
pfGrid w h = transform (pfGrid' w h)

-- I tried using mod to calculate xx, yy, xi, yi, but it was wrong, so I just inlined the original rust grid_fmod2()
pfGrid' :: E Float -> E Float -> Transformer
pfGrid' w h (Transform xy t) =
  let x = _x xy
      y = _y xy
      xow = sh $ x /^ w
      yoh = sh $ y /^ h
      xx = sh $ (xow -^ sfloor xow) * w
      xi = sh $ sfloor xow
      yy = sh $ (yoh -^ sfloor yoh) * h
      yi = sh $ sfloor yoh
      xx2 = sh $ Cond (smod (sabs xi) (KF 2.0) ==. (KF 1.0)) (w -^ xx) xx
      yy2 = sh $ Cond (smod (sabs yi) (KF 2.0) ==. (KF 1.0)) (h -^ yy) yy
   in Transform (V2 xx2 yy2) t
