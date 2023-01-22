module Single
( singleHandler ) where

import qualified Data.Map as M

import Compile
import E
import Template
import Util

compileBinding :: String -> E (V4 Float) -> String
compileBinding var e = compileE var e
-- compileBinding var e = concat [ty, " ", var, " = ", compileE e, ";"]
--   where ty = typeName e

black :: E (V4 Float)
black = V4 (KF 0.0) (KF 0.0) (KF 0.0) (KF 1.0)
white :: E (V4 Float)
white = V4 (KF 1.0) (KF 1.0) (KF 1.0) (KF 1.0)

smoothstep :: E Float -> E Float -> E Float -> E Float
smoothstep = Fun3 "smoothstep"

smooth :: E (V4 Float) -> E (V4 Float) -> E Float -> E (V4 Float)
smooth fg bg dist =
  let smoothRadius = KF 0.03
      bwBlend = sh $ smoothstep (-smoothRadius) smoothRadius dist
      color = (bwBlend *^ bg) +^ ((KF 1.0 -^ bwBlend) *^ fg);
   in sh color

singleHandler :: IO String
singleHandler = do
  let c = compileBinding "topColor" col
      col = smooth black white $ evalShape filaoa
  -- msp c -- slow
  html <- generateExe "single.html" $ M.fromList [("SHAPE_ASDF", c)]
  return html
