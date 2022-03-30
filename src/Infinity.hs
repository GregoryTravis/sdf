module Infinity
( getShapeFunction ) where

import qualified Data.Map as M

import Color
import Compile
import Random
import Template
import Transform
import Util

getShapeFunction :: IO String
getShapeFunction = do
  -- let pc = smooth white black $ evalShape spinner
  pc <- crecipes
  let glsl = compileFunction pc
  msp glsl
  return glsl
