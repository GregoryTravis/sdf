module Random
( randomShape
, recipe
, crecipes
, thang2
, spinner
, randSpinner ) where

import System.Random

import BinOp
import Color
import E
import Funs
import Grid
import Lib
import Prim
import Transform
import Util hiding (time)

rnd :: Random n => n -> n -> IO n
rnd a b = (getStdRandom (randomR (a, b)))

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

recipe :: IO Shape
recipe = randIO recipes
  where recipes = [
            thang
          , return filaoa
          , return anotherGreatOne
          , return undulum
          -- , return hmm
          ]

crecipe :: IO E
crecipe = do
  col <- randomMaybeTransparentColor 0.333
  r <- recipe
  let camera = scale 1.0
      color = smooth col nothing $ evalShape (camera r)
  return color

crecipes :: IO E
crecipes = do
  n <- getStdRandom (randomR (1::Int, 4))
  colors <- mapM (\_ -> crecipe) [0..n-1]
  return $ alphaBlends colors

-- crecipes :: IO E
-- crecipes = do
--   s' <- recipe
--   let s = difference (smoothUnion undulum s') s' -- (scale 0.2 (pfGrid 1.5 1.5 circle))
--       camera = scale 1.0
--       c = smooth white nothing $ evalShape $ camera s
--       cs = alphaBlends [black, c]
--   return cs

undulum :: Shape
undulum =
  let s' = belowFun (undulo 5 0.7 2.0)
      s'' = belowFun (undulo 5 1.0 1.5)
      -- s = intersection s' $ rotation (- (KF pi / 2.0)) s''
      rs' = translation (V2 0.0 (-0.3)) s'
      rs'' = translation (V2 0.5 0.0) $ r $ s'' -- translation (V2 0.0 (-3.0)) s''
      s = smoothUnion rs' rs''
      r = rotation (- (KF pi / 2.0))
   in s

undulo :: E -> E -> E -> E -> E -> E
undulo freq amp ampfreq t x =
  let amp' = amp * ssin (t * ampfreq)
   in amp' * ssin (freq * x)

-- f :: t -> x -> y
belowFun :: (E -> E -> E) -> Shape
belowFun f (Transform xy t) =
  let y = f t (X xy)
      dist = (Y xy) - y
   in dist


thang :: IO Shape
thang = do
  rs0 <- randomShape
  rs1 <- randomShape
  let rs0' = rotation (osc 0.5) (pfGrid 2 2 rs0)
  let rs1' = rotation (osc (-0.35)) (pfGrid 1.5 1.5 rs1)
  let p = interp (osc 0.2) rs0' rs1'
  return $ scale 0.1 p

thang2 :: IO E
thang2 = do
  a <- translation (V2 (time * 0.8) 0.0) <$> recipe 
  b <- rotation (time * 0.2) <$> recipe
  c <- recipe
  cola <- randomColor
  colb <- randomColor
  colc <- randomTransparentColor
  let camera = scale 0.1
      acolor = smooth cola nothing $ evalShape $ camera a
      bcolor = smooth colb nothing $ evalShape $ camera b
      ccolor = smooth colc nothing $ evalShape $ camera c
      color = alphaBlends [acolor, bcolor, ccolor]
  return color

-- FAVORITE don't lose this!!
-- fall in love all over again
filaoa :: Shape
filaoa = scale 0.1 $ smoothUnion (scale (time / 10.0) filaoa') (rotation (time / 10.0) filaoa')
  where filaoa' = pfGrid 2.25 2.25 circle

anotherGreatOne :: Shape
anotherGreatOne =
  let ss = rotation (time * 0.4) $ pfGrid 2.5 2.5 square
      cs = rotation (time * (-0.3)) $ pfGrid 2.5 2.5 circle
      gridz = interp (osc 0.5) ss cs
      morph = interp (osc 2) gridz filaoa
   in morph

-- bloop experiment
hmm :: Shape
hmm = do
  let cir = (translation (V2 (- (time * 0.08)) 0.0)) $ (scale 0.15) circle
      smaller = (translation (V2 (- (time * 0.08)) 0.0)) $ (scale 0.03) circle
      both = smoothUnion square cir
      p' = difference both cir
      p3 = union p' smaller
   in scale 0.1 p3

spinner :: Shape
spinner = scale 0.1 $ rotation (time * 4.0) $ translation (V2 3.0 0.0) circle

randSpinner :: IO Shape
randSpinner = do
  smallR <- KF <$> rnd 0.05 0.2
  largeR <- KF <$> rnd 0.2 0.5
  speed <- KF <$> rnd 2.0 5.0
  return $ rotation (time * speed) $ translation (V2 largeR 0.0) $ scale smallR circle
