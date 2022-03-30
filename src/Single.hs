module Single
( singleHandler ) where

import qualified Data.Map as M

import Compile
import Random
import Template
import Util

singleHandler :: IO String
singleHandler = do
  pc <- crecipes
  let c = compileSingle pc
  msp c
  html <- generateExe "single.html" $ M.fromList [("SHAPE_ASDF", c)]
  return html
