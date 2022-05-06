module Single
( singleHandler ) where

import qualified Data.Map as M

import Compile
import E
import Template
import Util

singleHandler :: E -> IO String
singleHandler pc = do
  let c = compileSingle pc
  -- msp c -- slow
  html <- generateExe "single.html" $ M.fromList [("SHAPE_ASDF", c)]
  return html
