{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.Map as M

import BinOp
import E
import Compile
import Lib
import Template
import Transform
import Util hiding (time)

grid :: E -> E -> UnOp
grid w h = transform (grid' w h)

grid' :: E -> E -> Transformer
grid' w h (Transform xy t) =
  let x = Sh $ X xy
      y = Sh $ Y xy
      xx = Sh $ smod x w
      yy = Sh $ smod y h
      xi = Sh $ sfloor x
      yi = Sh $ sfloor y
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
      xow = Sh $ x / w
      yoh = Sh $ y / h
      xx = Sh $ (xow - sfloor xow) * w
      xi = Sh $ sfloor xow
      yy = Sh $ (yoh - sfloor yoh) * h
      yi = Sh $ sfloor yoh
      xx2 = Sh $ Cond (smod (Abs xi) 2.0 ==. 1.0) (w - xx) xx
      yy2 = Sh $ Cond (smod (Abs yi) 2.0 ==. 1.0) (h - yy) yy
   in Transform (V2 xx2 yy2) t

-- fn grid_fmod2(a: f32, b: f32) -> (f32, i32) {
--   let aob = a / b;
--   (((aob - aob.floor()) * b), aob.floor() as i32)
-- }

    -- let (mut xx, xi) = grid_fmod2(x, w);
    -- let (mut yy, yi) = grid_fmod2(y, h);
    -- if xi.abs() % 2 == 1 {
    --   xx = w - xx;
    -- }
    -- if yi.abs() % 2 == 1 {
    --   yy = h - yy;
    -- }
    -- (xx, yy, t)

square :: Shape
square (Transform xy _) =
  let center = Sh $ V2 0.0 0.0
      radius = Sh 1.0
      sd = Sh $ Abs (xy - center)
      dist = Sh $ (Max (X sd) (Y sd) / radius) - 1.0
   in dist

circle :: Shape
circle (Transform xy _) =
  let dist = Length xy - 1.0
   in dist

main = do
  let camera = scale 0.1

  let rot = rotation $ 50.0 * time
      slide = translation (V2 (time * 0.8) 0.0)
      -- p = transform rot square
      srp = rot $ slide square
      rsp = slide $ rot square
      p2 = smoothUnion srp rsp

  -- let s = tsquare (XY / t) t
  -- let s = tsquare rotXY t
  -- let s = smu

  let cir = (translation (V2 (- (time * 0.8)) 0.0)) $ (scale 0.15) circle
      smaller = (translation (V2 (- (time * 0.8)) 0.0)) $ (scale 0.03) circle
      both = smoothUnion square cir
      p' = difference both cir
      p3 = union p' smaller

  -- let p = union p2 p3
  -- let p = pfGrid 2.25 2.25 square

  -- FAVORITE don't lose this!!
  -- fall in love all over again
  let filaoa' = pfGrid 2.25 2.25 circle
  let filaoa = smoothUnion (scale time filaoa') (rotation time filaoa')

  let p = filaoa

  let pc = camera p

  let s = evalShape pc

  let c = compile s
  msp c
  generateExe "template.html" "index.html" $ M.fromList [("SHAPE_ASDF", c)]
  msp "hi"
