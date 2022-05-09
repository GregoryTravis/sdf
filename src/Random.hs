{-# LANGUAGE FlexibleInstances, FlexibleContexts, FunctionalDependencies, GADTs , MultiParamTypeClasses, TypeOperators #-}

module Random
( randomShape
, recipe
, crecipes
, realRandom
, realRandomOsc
, realRandomPile ) where

import Control.Monad (join)
import Control.Monad.Random.Class
import Control.Monad.Random.Strict
import System.Random hiding (uniform)

import Alg
import BinOp
import Color
import E
import Funs
import Grid
import Lib
import Prim
import Transform
import Util hiding (die, time)

-- default (Double)

recipe :: Rnd Shape
recipe = uniformM
  [
    rLimonTwaist
  , thang
  , pure filaoa
  , rAnotherGreatOne
  , rvlad
  , rZinny
  ]

oscRecipe2 :: Rnd Shape -> Rnd Shape
oscRecipe2 r = interp (osc 0.5) <$> r <*> r

oscRecipe3 :: Rnd Shape -> Rnd Shape
oscRecipe3 r = interp (osc 0.5) <$> r <*> (interp <$> pure (osc 0.33) <*> r <*> r)

oscRecipe :: Rnd Shape -> Rnd Shape
oscRecipe r = uniformM [oscRecipe2 r, oscRecipe3 r]

coolShape :: IO E
coolShape = do
  r <- evalRandIO $ oscRecipe recipe
  return $ evalShape r

crecipe :: IO E -> IO E
crecipe shaper = do
  col <- randomMaybeTransparentColor 0.333
  shape <- shaper
  let color = smooth col nothing shape
  return color

crecipes :: IO E
crecipes = pile coolShape

pile :: IO E -> IO E
pile shaper = do
  -- determinisitic
  -- n <- return 1
  n <- getStdRandom (randomR (1::Int, 4))
  colors <- mapM (\_ -> crecipe shaper) [0..n-1]
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

classicZinny :: Rnd Shape
classicZinny = zinny <$> pure 10.0 <*> pure 5.0 <*> pure 1.5 <*> pure 1.5
rZinny :: Rnd Shape
rZinny = zinny <$> ps <*> ps <*> g <*> g
  where ps = 2.0...16.0
        g = 1.2...2.8

zinny :: E -> E -> E -> E -> Shape
zinny p0 p1 g0 g1 =
  let whoa = transform (flowerize p0) $ transform (whorl 3.0 10.0) $ transform (flowerize p1) (pfGrid g0 g0 circle)
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

classicVlad :: Rnd Shape
classicVlad = vlad <$> pure 1.5 <*> pure 1.0 <*> pure 1.0 <*> pure 1.0 <*> pure 1.0 <*> pure 0.25
rvlad = vlad <$> (1.0...1.8) <*> s <*> s <*> s <*> s <*> sc
  where s = 0.2...3.0
        sc = pure 0.25 -- 0.05...0.3

rLimonTwaist = limonTwaist <$> 1.1...1.7 <*> 1.0...6.0 <*> 3.0...12.0
limonTwaist :: E -> E -> E -> Shape
limonTwaist gs wf wa =
  let g = pfGrid gs gs circle
   in scale 0.25 $ transform (whorl wf wa) g

whorl :: E -> E -> Transformer
whorl wf wa tr@(Transform xy t) =
  let ang = Length xy * 2.0 * KF pi * (ssin (t / wf) / wa)
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

type Rnd a = Rand StdGen a

lo = randomR (KF 3.4, KF 4.5) :: StdGen -> (E, StdGen)
loo = liftRand $ lo :: Rand StdGen E
noo = evalRandIO loo
-- uniform :: (Foldable t, MonadRandom m) => t a -> m a
uoo = uniform [circle] :: Rnd Shape
soo = scale <$> loo <*> pure circle :: Rand StdGen Shape

(...) :: E -> E -> Rnd E
a ... b = liftRand $ randomR (a, b)

uniformM :: [Rnd a] -> Rnd a
uniformM rands = do
  rand <- uniform rands
  rand

randomPrim :: Rnd Shape
randomPrim = uniformM (map pure allPrims)

randomShape :: Rnd Shape
randomShape = uniformM
  [ randomPrim
  , randomUnOp <*> randomPrim
  , randomBinOp <*> randomPrim <*> randomPrim
  ]

randomUnOp :: Rnd UnOp
randomUnOp = uniformM
  [ scale <$> scalers
  , translation <$> translators
  , rotation <$> rotators
  , gridder
  , pfGridder
  ]

randomBinOp :: Rnd BinOp
randomBinOp = uniformM bos
  where bos = (map pure allBinOps) ++ [randInterp]
        randInterp = interp <$> (0.0...1.0)

scalers :: Rnd E
scalers = uniformM
  [ 0.25...4.0
  , osc <$> (0.25...4.0)
  ]

translators :: Rnd E
translators = uniformM
  [ V2 <$> small <*> small
  , V2 <$> small <*> pure (KF 0.0)
  , V2 <$> pure (KF 0.0) <*> small
  ]
  where small = ((-3.0)...3.0)

rotators :: Rnd E
rotators = uniformM
  [ (0.0...KF pi)
  ]

gridder :: Rnd UnOp
gridder = grid <$> dim <*> dim
  where dim = 1.5...3.0

pfGridder :: Rnd UnOp
pfGridder = pfGrid <$> dim <*> dim
  where dim = 1.5...3.0

thang :: Rnd Shape
thang = sspthang' <$> randomShape <*> randomShape <*> (0.1...1.2) <*> ((-0.5)...0.9) <*> (0.5...2.5) <*> (1.0...3.0) <*> (0.1...4.0)
sspthang' :: Shape -> Shape -> E -> E -> E -> E -> E -> Shape
sspthang' rs0 rs1 r0 r1 g0 g1 interpRate = do
  let rs0' = rotation (osc r0) (pfGrid g0 g0 rs0)
      rs1' = rotation (osc r1) (pfGrid g1 g1 rs1)
      p = interp (osc interpRate) rs0' rs1'
   in scale 0.1 p

-- FAVORITE don't lose this!!
-- fall in love all over again
filaoa :: Shape
filaoa = scale 0.1 $ smoothUnion (scale (time / 10.0) filaoa') (rotation (time / 10.0) filaoa')
  where filaoa' = pfGrid 2.25 2.25 circle

classicAnotherGreatOne :: Rnd Shape
classicAnotherGreatOne = anotherGreatOne <$> pure 0.4 <*> pure 2.5 <*> pure (-0.3) <*> pure 2.5 <*> pure 0.5 <*> pure 2

rAnotherGreatOne :: Rnd Shape
rAnotherGreatOne = anotherGreatOne <$> (0.1...0.6) <*> (0.8...3.2) <*> ((-1.5)...(-0.1)) <*> (0.8...3.2) <*> (0.2...0.5) <*> (2...3)

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

realRandom :: IO E
realRandom = do
  e <- shpEval <$> (eeesp "Shp" <$> (evalRandIO $ sizedProgram 4))
  return $ smooth white black $ evalShape (scale 0.25 e)

realRandomOsc :: IO E
realRandomOsc = undefined
-- realRandomOsc = do
--   e <- shpEval <$> (evalRandIO $ oscRecipe $ sizedProgram 4)
--   return $ smooth white black $ evalShape (scale 0.25 e)

realRandomPile :: IO E
realRandomPile = pile rr
  where rr = do
          e <- shpEval <$> (eeesp "Shp" <$> (evalRandIO $ sizedProgram 4))
          return $ evalShape (scale 0.25 e)

-- interp needs its own stacko
data Op = BO ShpBinOp | UO ShpUnOp
type Program = [Op]

-- Generate a random shape starting with N primitives as raw material
sizedProgram :: Int -> Rnd Shp
-- sizedProgram n = iterateSizedProgram <$> nPrims n
sizedProgram n = nPrims n >>= iterateSizedProgram 

nRnds :: Int -> Rnd a -> Rnd [a]
nRnds n r = mapM (\_ -> r) [0..n-1]

nPrims :: Int -> Rnd [Shp]
nPrims n = nRnds n nullOps

-- TODO should this be Rnd as well?
-- nPrims :: Int -> IO [Shape]
-- nPrims 0 = return []
-- nPrims n = do
--   s <- evalRandIO nullOps
--   ss <- nPrims (n - 1)
--   return $ s : ss

iterateSizedProgram :: [Shp] -> Rnd Shp
iterateSizedProgram [] = error "iterateSizedProgram: empty?"
iterateSizedProgram [p] = return p
iterateSizedProgram prims = do
  op <- randOp
  let prims' = applyOp op prims
  iterateSizedProgram prims'

-- TODO maybe we should pre-generate this with the right # of unops
randOp :: Rnd Op
randOp = uniformM [bs, us]
  where bs = BO <$> binOps
        us = UO <$> unOps

applyOp :: Op -> [Shp] -> [Shp]
applyOp (UO unop) (a : rest) = unop a : rest
applyOp (BO binop) (a : b : rest) = binop a b : rest

-- la = 3.0...8.0 :: Rnd E
-- TODO Rnd should be applicative, shouldn't it
flowerNullOp :: Rnd Shp
flowerNullOp = Flower <$> (flr <$> (3.0...8.0))
  where flr (KF n) = KF (fromIntegral $ floor n)
nullOps :: Rnd Shp
nullOps = gr <*> (uniformM [pure Circle, pure Square, flowerNullOp])
  where gr = PfGrid <$> grs <*> grs
        grs = 1.1...2.5
binOps :: Rnd ShpBinOp
binOps = uniformM [pure Union, pure Intersection, pure Difference, pure SmoothUnion, interpUnOp]
interpUnOp :: Rnd ShpBinOp
interpUnOp = Interp <$> 0.0...1.0
unOps :: Rnd ShpUnOp
unOps = uniformM [sc, tr, ro, gr]
  where sc = Scale <$> (osc <$> 0.5...2.0)
        tr = Translation <$> (V2 <$> t <*> t)
        t = osc <$> (-3.0)...3.0
        ro = Rotation <$> ang
        ang = osc <$> (KF (-pi))...(KF pi)
        gr = PfGrid <$> grs <*> grs
        grs = osc <$> 1.1...2.5
