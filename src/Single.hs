module Single
( singleHandler ) where

import qualified Data.Map as M

import Compile
import Color
import E
import Template
import Transform
import Util

-- smoothstep :: E Float -> E Float -> E Float -> E Float
-- smoothstep = Fun3 "smoothstep"

-- smooth :: E (V4 Float) -> E (V4 Float) -> E Float -> E (V4 Float)
-- smooth fg bg dist =
--   let smoothRadius = KF 0.03
--       bwBlend = sh $ smoothstep (-smoothRadius) smoothRadius dist
--       color = (bwBlend *^ bg) +^ ((KF 1.0 -^ bwBlend) *^ fg);
--    in sh color

singleHandler :: Color -> IO String
singleHandler pc = do
  let c = compileSingle pc
  -- let c = compileBinding "topColor" col
  --     col = smooth black white $ pc -- evalShape pc
  -- msp c -- slow
  html <- generateExe "single.html" $ M.fromList [("SHAPE_ASDF", c)]
  return html
