module Single
( singleHandler ) where

import qualified Data.Map as M

import Compile
import Color
import E
import Template
import Transform
import Util

singleHandler :: Color -> IO String
singleHandler pc = do
  let c = compileSingle pc
  -- let c = compileBinding "topColor" col
  --     col = smooth black white $ pc -- evalShape pc
  msp c -- slow
  html <- generateExe "single.html" $ M.fromList [("SHAPE_ASDF", c)]
  return html
