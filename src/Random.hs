{-# LANGUAGE FlexibleInstances, FlexibleContexts, FunctionalDependencies, GADTs , MultiParamTypeClasses, TypeOperators #-}

module Random
( randomShape
, recipe
, crecipes
, thang2 ) where

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

recipe :: IO Shape
recipe = randIO recipes
  where recipes = [
          --   deRnd thang
          -- , deRnd $ Here filaoa
          -- , deRnd rAnotherGreatOne
          -- , deRnd $ rvlad
            deRnd rZinny
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

classicZinny = zinny $. Here 10.0 *. Here 5.0 *. Here 1.5 *. Here 1.5
rZinny = zinny $. ps *. ps *. g *. g
  where ps = 2.0...16.0
        g = 0.8...2.8

zinny :: E -> E -> E -> E -> Shape
zinny p0 p1 g0 g1 =
  let whoa = transform (flowerize p0) $ transform whorl $ transform (flowerize p1) (pfGrid g0 g0 circle)
      g = translation (V2 time 0.0) $ pfGrid g1 g1  circle
   in scale 0.25 $ smoothUnion whoa g

-- Create two grids of the same shape, apply 2 sines (hor and vert) to one, and
-- smoothUnion them
vlad :: E -> E -> E -> E -> E -> E -> Shape
vlad gr sxa sxb sya syb sc =
  let g = pfGrid gr gr circle
      trx = transform (sinex sxa sxb time)
      try = transform (siney sya syb time)
      -- sg = rotation time $ trx $ try g
      sg = trx $ try $ rotation time g
   in scale sc $ smoothUnion g sg

classicVlad = vlad $. Here 1.5 *. Here 1.0 *. Here 1.0 *. Here 1.0 *. Here 1.0 *. Here 0.25
rvlad = vlad $. (1.0...1.8) *. s *. s *. s *. s *. sc
  where s = 0.2...3.0
        sc = Here 0.25 -- 0.05...0.3

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
  Range :: (Num n, Fractional n, Random n) => (n, n) -> Rnd n
  Choice :: [Rnd a] -> Rnd a
  RApp :: Rnd (a -> b) -> Rnd a -> Rnd b
  Here :: a -> Rnd a

deRnd :: Rnd a -> IO a
deRnd (Range lohi) = do
  n <- getStdRandom (randomR lohi)
  return n
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

(...) :: (Num n, Fractional n, Random n) => n -> n -> Rnd n
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
  [ (0.0...KF pi)
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

classicAnotherGreatOne :: IO Shape
classicAnotherGreatOne = deRnd $ anotherGreatOne $. Here 0.4 *. Here 2.5 *. Here (-0.3) *. Here 2.5 *. Here 0.5 *. Here 2

rAnotherGreatOne :: Rnd Shape
rAnotherGreatOne = anotherGreatOne $. (0.1...0.6) *. (0.8...3.2) *. ((-1.5)...(-0.1)) *. (0.8...3.2) *. (0.2...0.5) *. (2...3)

anotherGreatOne :: E -> E -> E -> E -> E -> E -> Shape
anotherGreatOne sr sg cr cg go mo =
  let ss = rotation (time * sr) $ pfGrid sg sg square
      cs = rotation (time * cr) $ pfGrid cg cg circle
      gridz = interp (osc go) ss cs
      morph = interp (osc mo) gridz filaoa
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
