module Grid
( grid
, pfGrid ) where

import E
import Lib
import Transform

grid :: E -> E -> UnOp
grid w h = transform (grid' w h)

grid' :: E -> E -> Transformer
grid' w h (Transform xy t) =
  let x = sh $ X xy
      y = sh $ Y xy
      xx = sh $ smod x w
      yy = sh $ smod y h
      xi = sh $ sfloor x
      yi = sh $ sfloor y
   in Transform (V2 xx yy) t

bugPfGrid :: E -> E -> UnOp
bugPfGrid w h = transform (bugPfGrid' w h)

bugPfGrid' :: E -> E -> Transformer
bugPfGrid' w h (Transform xy t) =
  let x = X xy
      y = Y xy
      xx = smod x w
      yy = smod y h
      xi = sfloor x
      yi = sfloor y
      xx2 = Cond (smod (Abs xi) 2 ==. 1) (w - xx) xx
      yy2 = Cond (smod (Abs yi) 2 ==. 1) (h - yy) yy
   in Transform (V2 xx2 yy2) t

bugPfGrid2 :: E -> E -> UnOp
bugPfGrid2 w h = transform (bugPfGrid2' w h)

bugPfGrid2' :: E -> E -> Transformer
bugPfGrid2' w h (Transform xy t) =
  let x = X xy
      y = Y xy
      xx = smod x w
      yy = smod y h
      xi = sfloor x
      yi = sfloor y
      xx2 = Cond (smod xi 2 ==. 1) (w - xx) xx
      yy2 = Cond (smod yi 2 ==. 1) (h - yy) yy
   in Transform (V2 xx2 yy2) t

pfGrid :: E -> E -> UnOp
pfGrid w h = transform (pfGrid' w h)

-- I tried using mod to calculate xx, yy, xi, yi, but it was wrong, so I just inlined the original rust grid_fmod2()
pfGrid' :: E -> E -> Transformer
pfGrid' w h (Transform xy t) =
  let x = X xy
      y = Y xy
      xow = sh $ x / w
      yoh = sh $ y / h
      xx = sh $ (xow - sfloor xow) * w
      xi = sh $ sfloor xow
      yy = sh $ (yoh - sfloor yoh) * h
      yi = sh $ sfloor yoh
      xx2 = sh $ Cond (smod (Abs xi) 2.0 ==. 1.0) (w - xx) xx
      yy2 = sh $ Cond (smod (Abs yi) 2.0 ==. 1.0) (h - yy) yy
   in Transform (V2 xx2 yy2) t
