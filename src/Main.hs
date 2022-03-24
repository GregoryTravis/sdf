{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.Map as M

import BinOp
import E
import Compile
import Grid
import Lib
import Prim
import Random
import Server
import Template
import Transform
import Util hiding (time)

genShape :: IO Shape
genShape = do
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

  -- oscillate 0..1 at 'rate'*2 hz
  let osc rate = Sh $ (ssin (time * rate) + 1) / 2

  -- let sc = interp osc square circle
  --     osc = (ssin (time * 30) + 1) / 2
  --     leftCircle = translation (V2 (-2) 0) circle
  --     rightCircle = translation (V2 (2) 0) circle
  --     circleMove = interp osc leftCircle rightCircle

  -- let morph = interp osc p2 filaoa
  --     osc = (ssin (time * 20) + 1) / 2 

  -- Another great one
  let ss = rotation (time * 4) $ pfGrid 2.5 2.5 square
      cs = rotation (time * (-3)) $ pfGrid 2.5 2.5 circle
      gridz = interp (osc 5) ss cs
      morph = interp (osc 20) gridz filaoa

  rs0 <- randomShape
  rs1 <- randomShape
  let rs0' = rotation (osc 5) (pfGrid 2 2 rs0)
  let rs1' = rotation (osc (-3.5)) (pfGrid 1.5 1.5 rs1)
  let p = interp (osc 2) rs0' rs1'

  let pc = camera p
  return pc

handler :: IO String
handler = do
  pc <- genShape
  let c = compile pc
  msp c
  html <- generateExe "template.html" $ M.fromList [("SHAPE_ASDF", c)]
  return html

main :: IO ()
main = runServer handler
