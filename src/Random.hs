{-# LANGUAGE FlexibleInstances, FlexibleContexts, FunctionalDependencies, GADTs , MultiParamTypeClasses, TypeOperators #-}

module Random
( randomShape
, recipe
, crecipes
, thang2
, spinner
, randSpinner ) where

import Control.Monad (join)
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

-- default (Double)

rnd :: Random n => n -> n -> IO n
rnd a b = (getStdRandom (randomR (a, b)))

recipe :: IO Shape
recipe = randIO recipes
  where recipes = [
            deRnd thang
          -- , return filaoa
          -- , return anotherGreatOne
          -- -- , return undulum
          -- , vlad <$> randomPrim
          -- , return zinny
          -- -- , return hmm
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
  -- determinisitic
  n <- return 1
  -- n <- getStdRandom (randomR (1::Int, 4))
  colors <- mapM (\_ -> crecipe) [0..n-1]
  return $ alphaBlends colors

determinisitic :: IO ()
determinisitic = do
  setStdGen $ mkStdGen 123

_crecipes :: IO E
_crecipes = do
  -- let s = trx $ try circle
  --     trx = transform sinex
  --     try = transform siney
  -- let s = vlad circle
  -- let s = limonTwaist
  -- let s =  transform (flowerize 5.0) limonTwaist -- (pfGrid 1.5 1.5 circle)
  let s = scale 0.25 $ transform (siney 0.1 10 time) $ transform (siney 1 1 time) $ pfGrid 1.5 1.5 circle
  return $ justShape s

zinny :: Shape
zinny =
  let whoa = transform (flowerize 10.0) $ transform whorl $ transform (flowerize 5.0) (pfGrid 1.5 1.5 circle)
      g = translation (V2 time 0.0) $ pfGrid 1.5 1.5 circle
   in scale 0.25 $ smoothUnion whoa g

-- Create two grids of the same shape, apply 2 sines (hor and vert) to one, and
-- smoothUnion them
vlad :: Shape -> Shape
vlad s =
  let g = pfGrid 1.5 1.5 s
      trx = transform (sinex 1 1 time)
      try = transform (siney 1 1 time)
      -- sg = rotation time $ trx $ try g
      sg = trx $ try $ rotation time g
   in scale 0.25 $ smoothUnion g sg

limonTwaist :: Shape
limonTwaist =
  let g = pfGrid 1.5 1.5 circle
   in scale 0.25 $ transform whorl g

whorl :: Transformer
whorl tr@(Transform xy t) =
  let ang = Length xy * 2.0 * KF pi * (ssin (t / 3.0) / 10.0)
   in rotation' ang tr

flowerize :: E -> Transformer
flowerize numPetals (Transform xy t) =
  let ang = Sh $ satan (Y xy) (X xy)
      mod = ssin t * (ssin $ (ang * numPetals))
      xy' = xy * (1.0 + (mod / 4.0))
   in Transform xy' t

sinex :: E -> E -> E -> Transformer
sinex amp freq phase (Transform xy t) =
  let x' = x + (amp * ssin (freq * y + phase))
      x = X xy
      y = Y xy
      xy' = V2 x' y
   in Transform xy' t

siney :: E -> E -> E -> Transformer
siney amp freq phase (Transform xy t) =
  let y' = y + (amp * ssin (freq * x + phase))
      x = X xy
      y = Y xy
      xy' = V2 x y'
   in Transform xy' t

justShape :: Shape -> E
justShape s =
  let c = smooth white nothing $ evalShape $ scale 0.5 s
   in alphaBlends [black, c]

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

-- pthang :: E -> E -> E -> E -> E -> IO Shape
-- pthang r0 r1 g0 g1 interpRate = do
--   rs0 <- randomShape
--   rs1 <- randomShape
--   let rs0' = rotation (osc r0) (pfGrid g0 g0 rs0)
--   let rs1' = rotation (osc r1) (pfGrid g1 g1 rs1)
--   let p = interp (osc interpRate) rs0' rs1'
--   return $ scale 0.1 p

randIO :: [IO a] -> IO a
randIO ios = do
  io <- randFromList ios
  io

data Rnd a where
  -- Range :: Num n => (n, n) -> Rnd E
  Range :: (Double, Double) -> Rnd E
  Choice :: [Rnd a] -> Rnd a
  RApp :: Rnd (a -> b) -> Rnd a -> Rnd b
  Here :: a -> Rnd a

deRnd :: Rnd a -> IO a
deRnd (Range lohi) = do
  n <- getStdRandom (randomR lohi)
  return $ KF n
deRnd (Choice rnds) = do
  rnd <- randFromList rnds
  deRnd rnd
deRnd (RApp rf ra) = do
  f <- deRnd rf
  a <- deRnd ra
  return $ f a
deRnd (Here a) = return a

infixl 4 $.
($.) :: (a -> b) -> Rnd a -> Rnd b
f $. r = Here f *. r

infixl 4 *.
(*.) :: Rnd (a -> b) -> Rnd a -> Rnd b
(*.) = RApp

(...) :: Double -> Double -> Rnd E
a ... b = Range (a, b)

randomPrim :: Rnd Shape
randomPrim = Choice (map Here allPrims)

randomShape :: Rnd Shape
randomShape = Choice
  [ randomPrim
  , randomUnOp *. randomPrim
  , randomBinOp *. randomPrim *. randomPrim
  ]

randomUnOp :: Rnd UnOp
randomUnOp = Choice
  [ scale $. scalers
  , translation $. translators
  , rotation $. rotators
  , gridder
  , pfGridder
  ]

randomBinOp :: Rnd BinOp
randomBinOp = Choice bos
  where bos = (map Here allBinOps) ++ [randInterp]
        randInterp = interp $. (0.0...1.0)

scalers :: Rnd E
scalers = Choice
  [ 0.25...4.0
  , osc $. (0.25...4.0)
  ]

translators :: Rnd E
translators = Choice
  [ V2 $. small *. small
  , V2 $. small *. Here (KF 0.0)
  , V2 $. Here (KF 0.0) *. small
  ]
  where small = ((-3.0)...3.0)

rotators :: Rnd E
rotators = Choice
  [ (0.0...pi)
  ]

gridder :: Rnd UnOp
gridder = grid $. dim *. dim
  where dim = 1.5...3.0

pfGridder :: Rnd UnOp
pfGridder = pfGrid $. dim *. dim
  where dim = 1.5...3.0

thang :: Rnd Shape
thang = sspthang' $. randomShape *. randomShape *. (0.1...1.2) *. ((-0.5)...0.9) *. (0.5...2.5) *. (1.0...3.0) *. (0.1...4.0)
sspthang' :: Shape -> Shape -> E -> E -> E -> E -> E -> Shape
sspthang' rs0 rs1 r0 r1 g0 g1 interpRate = do
  let rs0' = rotation (osc r0) (pfGrid g0 g0 rs0)
      rs1' = rotation (osc r1) (pfGrid g1 g1 rs1)
      p = interp (osc interpRate) rs0' rs1'
   in scale 0.1 p

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
