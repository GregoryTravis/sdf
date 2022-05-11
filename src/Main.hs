{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.Map as M

import BinOp
import E
import Color
import Compile
import Funs
import Grid
import Lib
import Prim
import Random
import Server
import Template
import Transform
import Util hiding (time)

genShape :: IO E
genShape = do
  crecipes

-- handler :: IO String
-- handler = do
--   let pc = smooth white black $ evalShape spinner
--   let c = compile pc
--   msp c
--   html <- generateExe "single.html" $ M.fromList [("SHAPE_ASDF", c)]
--   return html

main :: IO ()
main = do
  noBuffering
  randomTiming
  -- runServer
