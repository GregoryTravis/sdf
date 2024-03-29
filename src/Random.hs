{-# LANGUAGE FlexibleInstances, FlexibleContexts, FunctionalDependencies, GADTs, MultiParamTypeClasses, TypeOperators #-}

module Random
( randomShape
, recipe
, crecipes
, outlinecrecipes
, realRandom
, realRandomOsc
, realRandomPile
, interpo1
, interpoPile
, anOutlineE
, aCircle
, legg
, randomCommander ) where

import Control.Monad (join)
import Control.Monad.Random.Class
import Control.Monad.Random.Strict
import Criterion.Main
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Random hiding (uniform)
import Text.Read (readMaybe)

import Alg
import BinOp
import Color
import Commander
import E
import Funs
import Grid
import Lib
import Prim
import Transform
import Util hiding (die, time)

-- default (Double)

-- sinex :: E Float -> E Float -> E Float -> Transformer
-- sinex amp freq phase (Transform xy t) =
--   let x' = x +^ (amp *^ ssin (freq *^ y +^ phase))
--       x = _x xy
--       y = _y xy
--       xy' = V2 x' y
--    in Transform xy' t

skwar :: Transformer
skwar (Transform xy t) =
  let x = _x xy
      y = _y xy
      xy' = V2 (x*x) (y*y)
   in Transform xy' t

legg :: IO Color
legg = do
  let t = transform skwar
      s = scale 0.2 $ t $ pfGrid 1.3 1.3 $ scale 0.5 square
  return $ smooth white black $ evalShape $ s
  -- let t = transform skwar
  --     sr = 1.0
  -- return $ smooth white black $ evalShape $ t $ rotation (time * sr) $ scale 0.5 square

primPick :: Commander Shape
primPick = mapCvt $ M.fromList
  [ ("circle", circle)
  , ("square", square)
  , ("flower", flower 4.0) ]

instance Show Shape where
  show _ = "<shape>"

-- e.g. http://localhost:8000/sizes+circle+1.0+1.0+1.0
randomCommander :: Commander (IO Color)
randomCommander = nest $ M.fromList
  [ ("sizes", sizes <$> via KF <*> via KF <*> via KF <*> primPick)
  -- , ("filaoa", pure $ crecipe $ return $ evalShape filaoa)
  , ("filaoa", pure $ return $ bandy black white $ evalShape filaoa)
  , ("vlad", pure $ (evalRandIO rvlad) >>= (\s -> return $ siBandy black white $ evalShape s))
  , ("hrLimonTwaist", pure $ (evalRandIO hrLimonTwaist) >>= (\s -> return $ smooth black white $ evalShape s))
  , ("winky", pure $ return $ smooth black white $ evalShape $ scale 0.1 winky)
  ]

winky :: Shape
winky =
  let gr = pfGrid 1.3 1.3 circle
      tra = sinex 1.0 2.0 time
      trb = sinex 1.9 os time
      os = (osc 2.0 * 0.4) + 1.8
      csa = transform tra gr
      csb = transform trb gr
   in intersection csa csb

-- a few circles actually
sizes :: E Float -> E Float -> E Float -> Shape -> IO Color
sizes rotvel xvel yvel s = return $ bandy white black $ evalShape $ trans shapes
   where shapes = union vvbig $ union vbig $ union tiny2 $ union tiny $ union smaller $ union medium $ union big small
         big = translation (V2 1.5 0.0) s
         vbig = translation (V2 (-0.58) 4.58) (scale 4 s)
         vvbig = translation (V2 (-0.63) (-8.8)) (scale 8 s)
         medium = scale 0.5 s
         smaller = translation (V2 (-1.0) 0.0) (scale 0.125 s)
         tiny = translation (V2 (-0.7) 0.0) (scale (1.0 / 16.0) s)
         tiny2 = translation (V2 (-0.6) 0.0) (scale (1.0 / 32.0) s)
         small = translation (V2 (-1.5) 0.0) (scale 0.25 s)
         trans s = translation (V2 (xvel * time) (yvel * time)) $ rotation (rotvel * time) $ s

aCircle = sizes 0.1 0.1 0.0 circle -- $ flower 4.0

recipe :: Rnd Shape
recipe = uniformM
  [
    rLimonTwaist
  , hrLimonTwaist
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

interpo :: IO (E Float)
interpo = do
  -- [a, b, c, d] <- (evalRandIO $ nRand recipe 4) >>= randColorsFor
  [a, b, c, d] <- (evalRandIO $ nRand recipe 4)
  -- let
  -- [a, b, c, d] = [square, circle, square, circle]
  -- let ae = evalShape a
  --     be = evalShape b
  --     ce = evalShape c
  --     de = evalShape d
  let top = interp (_x mouse) a b
      bot = interp (_x mouse) c d
      both = interp (_y mouse) top bot
  return $ evalShape both

interpo1 :: IO Color
interpo1 = do
  s <- interpo
  return $ smooth white black s

interpoPile :: IO Color
interpoPile = pile 2 interpo

randColorsFor :: [Shape] -> IO [Color]
randColorsFor shapes = mapM randColorFor shapes
randColorFor :: Shape -> IO Color
randColorFor shape = do
  col <- randomMaybeTransparentColor 0.333
  let color = smooth col nothing (evalShape shape)
  return color

coolShape :: IO Dist
coolShape = do
  r <- evalRandIO $ oscRecipe recipe
  return $ evalShape r
  -- return $ outline 0.05 0.15 $ evalShape r

outlineCoolShape :: IO Dist
outlineCoolShape = outline 0.05 0.15 <$> coolShape

crecipe :: IO Dist -> IO Color
crecipe shaper = do
  col <- randomMaybeTransparentColor 0.333
  shape <- shaper
  let color = smooth col nothing shape
  -- let color = bandy black white shape
  return color

outlinecrecipes :: IO Color
outlinecrecipes = pile 2 outlineCoolShape

crecipes :: IO Color
crecipes = pile 4 coolShape

pile :: Int -> IO Dist -> IO Color
pile maxN shaper = do
  -- determinisitic
  -- n <- return 1
  n <- getStdRandom (randomR (1, maxN))
  colors <- mapM (\_ -> crecipe shaper) [0..n-1]
  return $ alphaBlends colors

determinisitic :: IO ()
determinisitic = do
  setStdGen $ mkStdGen 123

_crecipes :: IO Color
_crecipes = do
  -- let s = trx $ try circle
  --     trx = transform sinex
  --     try = transform siney
  -- let s = vlad circle
  -- let s = limonTwaist
  -- let s =  transform (flowerize 5.0) limonTwaist -- (pfGrid 1.5 1.5 circle)
  let s = scale 0.25 $ transform (siney 0.1 10 time) $ transform (siney 1 1 time) $ pfGrid 1.5 1.5 circle
  return $ justShape s

anOutline :: E Float
anOutline =
  let thing = vlad 1.5 1.0 1.0 1.0 1.0 0.25
      things = smoothUnion (scale 0.5 thing) (rotation time thing)
  in outline 0.02 0.05 $ evalShape $ things
  -- let trx = transform (sinex 1.5 1.0 time)
  --     try = transform (siney 1.0 1.0 time)
  -- in outline 0.02 0.05 $ evalShape $ trx $ try $ scale 0.25 $ pfGrid 1.5 1.5 circle

-- Distance -> distance
edge :: E Float -> E Float -> E Float
edge w x = (abs (x + w)) - w

outline :: E Float -> E Float -> E Float -> E Float
outline lineWidth gap x =
  let outer = edge lineWidth (x -^ (gap /^ KF 2.0))
      inner = edge lineWidth (x +^ (gap /^ KF 2.0))
   in smin outer inner

anOutlineE :: IO Color
anOutlineE = return $ smooth white black anOutline

classicZinny :: Rnd Shape
classicZinny = zinny <$> pure 10.0 <*> pure 5.0 <*> pure 1.5 <*> pure 1.5
rZinny :: Rnd Shape
rZinny = zinny <$> ps <*> ps <*> g <*> g
  where ps = 2.0...16.0
        g = 1.2...2.8

zinny :: E Float -> E Float -> E Float -> E Float -> Shape
zinny p0 p1 g0 g1 =
  let whoa = transform (flowerize p0) $ transform (whorl 3.0 10.0) $ transform (flowerize p1) (pfGrid g0 g0 circle)
      g = translation (V2 time 0.0) $ pfGrid g1 g1  circle
   in scale 0.25 $ smoothUnion whoa g

-- Create two grids of the same shape, apply 2 sines (hor and vert) to one, and
-- smoothUnion them
vlad :: E Float -> E Float -> E Float -> E Float -> E Float -> E Float -> Shape
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

-- rLimonTwaist :: RandT StdGen Data.Functor.Identity.Identity Shape
rLimonTwaist :: Rnd Shape
rLimonTwaist = limonTwaist <$> 1.1...1.7 <*> 1.0...6.0 <*> 3.0...12.0
limonTwaist :: E Float -> E Float -> E Float -> Shape
limonTwaist gs wf wa =
  let g = pfGrid gs gs circle
   in scale 0.25 $ transform (whorl wf wa) g

hrLimonTwaist :: Rnd Shape
hrLimonTwaist = hlimonTwaist <$> 1.1...1.7 <*> 1.0...6.0 <*> 3.0...12.0
hlimonTwaist :: E Float -> E Float -> E Float -> Shape
hlimonTwaist gs wf wa =
  let g = pfGrid gs gs circle
   in scale 0.25 $ transform (hwhorl wf wa) g

whorl :: E Float -> E Float -> Transformer
whorl wf wa tr@(Transform xy t) =
  let ang = Length xy * 2.0 * KF pi * (ssin (t / wf) / wa)
   in rotation' ang tr

-- Rotation varies as x, not dist
hwhorl :: E Float -> E Float -> Transformer
hwhorl wf wa tr@(Transform xy t) =
  -- let ang = ssin (X xy - Y xy) * 2.0 * KF pi * (ssin (t / wf) / wa)
  let ang = ssin (_x xy) - scos (_y xy) * 5.0 * KF pi * (ssin (t / wf) / wa)
   in rotation' ang tr
-- hwhorl _ _ (Transform e _) = error $ "huh " ++ show e

flowerize :: E Float -> Transformer
flowerize numPetals (Transform xy t) =
  let ang = sh $ satan (_y xy) (_x xy)
      mod = ssin t * (ssin $ (ang * numPetals))
      xy' = xy *^ (KF 1.0 +^ (mod /^ KF 4.0))
   in Transform xy' t

sinex :: E Float -> E Float -> E Float -> Transformer
sinex amp freq phase (Transform xy t) =
  let x' = x +^ (amp *^ ssin (freq *^ y +^ phase))
      x = _x xy
      y = _y xy
      xy' = V2 x' y
   in Transform xy' t

siney :: E Float -> E Float -> E Float -> Transformer
siney amp freq phase (Transform xy t) =
  let y' = y +^ (amp *^ ssin (freq *^ x +^ phase))
      x = _x xy
      y = _y xy
      xy' = V2 x y'
   in Transform xy' t

justShape :: Shape -> Color
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

undulo :: E Float -> E Float -> E Float -> E Float -> E Float -> E Float
undulo freq amp ampfreq t x =
  let amp' = amp * ssin (t * ampfreq)
   in amp' * ssin (freq * x)

-- f :: t -> x -> y
belowFun :: (E Float -> E Float -> E Float) -> Shape
belowFun f (Transform xy t) =
  let y = f t (_x xy)
      dist = (_y xy) - y
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

-- TODO: I know, I know
nRand :: Rnd a -> Int -> Rnd [a]
nRand rnd 0 = return []
nRand rnd n = do
  x <- rnd
  xs <- nRand rnd (n-1)
  return $ x:xs

type Rnd a = Rand StdGen a

-- lo = randomR (KF 3.4, KF 4.5) :: StdGen -> (E, StdGen)
-- loo = liftRand $ lo :: Rand StdGen E
-- noo = evalRandIO loo
-- -- uniform :: (Foldable t, MonadRandom m) => t a -> m a
-- uoo = uniform [circle] :: Rnd Shape
-- soo = scale <$> loo <*> pure circle :: Rand StdGen Shape

(...) :: E Float -> E Float -> Rnd (E Float)
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

scalers :: Rnd (E Float)
scalers = uniformM
  [ 0.25...4.0
  , osc <$> (0.25...4.0)
  ]

translators :: Rnd (E (V2 Float))
translators = uniformM
  [ V2 <$> small <*> small
  , V2 <$> small <*> pure (KF 0.0)
  , V2 <$> pure (KF 0.0) <*> small
  ]
  where small = ((-3.0)...3.0)

rotators :: Rnd (E Float)
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
sspthang' :: Shape -> Shape -> E Float -> E Float -> E Float -> E Float -> E Float -> Shape
sspthang' rs0 rs1 r0 r1 g0 g1 interpRate = do
  let rs0' = rotation (osc r0) (pfGrid g0 g0 rs0)
      rs1' = rotation (osc r1) (pfGrid g1 g1 rs1)
      p = interp (osc interpRate) rs0' rs1'
   in scale 0.1 p

-- FAVORITE don't lose this!!
-- fall in love all over again
filaoa :: Shape
filaoa = scale (KF 0.1) $ smoothUnion (scale (time /^ (KF 10.0)) filaoa') (rotation (time /^ (KF 10.0)) filaoa')
  where filaoa' = pfGrid (KF 2.25) (KF 2.25) circle

classicAnotherGreatOne :: Rnd Shape
classicAnotherGreatOne = anotherGreatOne <$> pure 0.4 <*> pure 2.5 <*> pure (-0.3) <*> pure 2.5 <*> pure 0.5 <*> pure 2

rAnotherGreatOne :: Rnd Shape
rAnotherGreatOne = anotherGreatOne <$> (0.1...0.6) <*> (0.8...3.2) <*> ((-1.5)...(-0.1)) <*> (0.8...3.2) <*> (0.2...0.5) <*> (2...3)

anotherGreatOne :: E Float -> E Float -> E Float -> E Float -> E Float -> E Float -> Shape
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

repeatable :: Maybe Int -> IO ()
repeatable (Just seed) = do
  msp ("repeatable", seed)
  setStdGen $ mkStdGen seed
  sg <- getStdGen
  msp ("stdgen", sg)
repeatable Nothing = do
  seed <- getStdRandom (randomR (1::Int, 100000))
  msp ("repeatable", seed)
  setStdGen $ mkStdGen seed
  sg <- getStdGen
  msp ("stdgen", sg)

-- rnd Shp
-- evalRandIO
-- shpEval
-- share
-- compile
-- randomTiming :: IO ()
-- randomTiming = do
--   let benchmark = env setupEnv $ \ ~e -> bench "bench" (nf share e)
--   defaultMainWith config [benchmark]
--   msp "hi randomTiming"
--   where config = defaultConfig -- { resamples = 1 }

setupEnv :: IO Dist
setupEnv = do
  repeatable (Just 33837) -- Nothing
  shp <- evalRandIO $ sizedProgram 6
  let shape = shpEval shp
      e = evalShape shape
  return e

realRandom :: IO Color
realRandom = do
  e <- shpEval <$> (eeesp "Shp" <$> (evalRandIO $ sizedProgram 4))
  return $ smooth white black $ evalShape (scale 0.25 e)

realRandomOsc :: IO Color
realRandomOsc = undefined
-- realRandomOsc = do
--   e <- shpEval <$> (evalRandIO $ oscRecipe $ sizedProgram 4)
--   return $ smooth white black $ evalShape (scale 0.25 e)

realRandomPile :: IO Color
realRandomPile = pile 4 rr
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
  where flr :: E Float -> E Float
        flr (KF n) = KF (fromIntegral $ floor n)
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
