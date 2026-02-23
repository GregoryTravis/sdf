module Single
( singleHandler ) where

import qualified Data.Map as M

import Compile
import Color
import E
import Template
import Transform
import Util

buildVarses :: [Color] -> [M.Map String String]
buildVarses cs =
  let num = length cs
      compiled = map compileSingle cs
      squareSide = ceiling (sqrt $ fromIntegral num)
      is = [0..]
      layouts = take num $ map (makeLayout squareSide) is
      chunks = zipWith3 makeChunk compiled is layouts
   in chunks
   where makeChunk c i (l, t, w, h) =
           M.fromList [
               ("SHAPE_ASDF", c)
               , ("CANVAS_ID_ASDF", "c" ++ show i)
               , ("FS_ID_ASDF", "fs" ++ show i)
               , ("LEFT_ASDF", show l)
               , ("TOP_ASDF", show t)
               , ("WIDTH_ASDF", show w)
               , ("HEIGHT_ASDF", show h)
             ]

-- These don't cover the whole region, e.g. if 3x3, all canvases will be 33vh,
-- leaving 1 in the margin.
makeLayout :: Int -> Int -> (Int, Int, Int, Int)
makeLayout side i =
  let sideInt = floor (100.0 / fromIntegral side)
      gx = i `mod` side
      gy = i `div` side
      left = gx * sideInt
      top = gy * sideInt
      width = sideInt
      height = sideInt
   in (left, top, width, height)

singleHandler :: Color -> IO String
singleHandler pc = do
  let cs = [pc, pc, pc, pc]
      varses = buildVarses cs
  --mapM msp cs -- slow
  htmls <- (mapM (generateExe "single.html") varses) :: IO [String]
  let html = concat htmls
  return html
