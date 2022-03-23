module Random
( randomShape ) where

import System.Random

import BinOp
import E
import Funs
import Grid
import Prim
import Transform
import Util

randomPrim :: IO Shape
randomPrim = randFromList allPrims

randomUnOp :: IO UnOp
randomUnOp = randIO unOps

randomBinOp :: IO BinOp
randomBinOp = randIO binOps

unOps :: [IO UnOp]
unOps = [
    randomize1 scale scalers
  , randomize1 translation translators
  , randomize1 rotation rotators
  , randomize2 grid gridders gridders
  , randomize2 pfGrid gridders gridders
  ]

binOps :: [IO BinOp]
binOps = (map return allBinOps) ++
  [ interp <$> (KF <$> (getStdRandom (randomR (0.0, 0.1))))
  ]

scalers :: [IO E]
scalers = [
    KF <$>  (getStdRandom (randomR (0.25, 4.0)))
  , osc <$> KF <$> (getStdRandom (randomR (2.0, 8.0)))
  ]

translators :: [IO E]
translators = [
    V2 <$> (KF <$> small) <*> (KF <$> small)
  , V2 <$> (KF <$> small) <*> (return $ KF 0.0)
  , V2 <$> (return $ KF 0.0) <*> (KF <$> small)
  ]
  where small :: IO Double
        small = getStdRandom (randomR ((-3.0), 3.0))

rotators :: [IO E]
rotators = [
    KF <$> getStdRandom (randomR (0, pi))
  ]

gridders :: [IO E]
gridders = [
    KF <$> dim
  ]
  where dim = getStdRandom (randomR (1.5, 3.0))

randIO :: [IO a] -> IO a
randIO ios = do
  io <- randFromList ios
  io

randomize1 :: (a -> b) -> [IO a] -> IO b
randomize1 f ioas = do
  a <- randIO ioas
  return (f a)

randomize2 :: (a -> b -> c) -> [IO a] -> [IO b] -> IO c
randomize2 f ioas iobs = do
  a <- randIO ioas
  b <- randIO iobs
  return (f a b)

randomShape :: IO Shape
randomShape = randIO randomShapes
randomShapes :: [IO Shape]
randomShapes = [
    randomPrim
  , (randomUnOp <*>) randomPrim
  , (randomBinOp <*>) randomPrim <*> randomPrim
  ]
