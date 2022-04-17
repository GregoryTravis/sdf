-- {-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeOperators #-}

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

-- default (Double)

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
            lpthang'
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
  n <- return 1 -- getStdRandom (randomR (1::Int, 4))
  colors <- mapM (\_ -> crecipe) [0..n-1]
  return $ alphaBlends colors

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

pthang :: E -> E -> E -> E -> E -> IO Shape
pthang r0 r1 g0 g1 interpRate = do
  rs0 <- randomShape
  rs1 <- randomShape
  let rs0' = rotation (osc r0) (pfGrid g0 g0 rs0)
  let rs1' = rotation (osc r1) (pfGrid g1 g1 rs1)
  let p = interp (osc interpRate) rs0' rs1'
  return $ scale 0.1 p

class Rend r a where
  toE :: r -> IO a

instance Rend Range E where
  toE (Range (lo, hi)) = do
    n <- getStdRandom (randomR (lo, hi))
    return $ KF n

-- instance Rend (Double, Double) E where
--   toE (lo, hi) = do
--     n <- getStdRandom (randomR (lo, hi))
--     return $ KF n

-- instance (Random n, Fractional n, Real n) => Rend (n, n) E where
--   toE (lo, hi) = do
--     n <- getStdRandom (randomR (lo, hi))
--     return $ KF $ realToFrac n

looft :: Rend r E => IO (E -> b) -> r -> IO b
looft iof rend = do
  f <- iof
  e <- toE rend
  return $ f e

-- data Range = Range Double Double
(...) :: Double -> Double -> Range
a ... b = Range (a, b)

-- lpthang = looft (looft (return pthang) (0.1...1.2)) ((-0.5)...0.9)
lpthang = looft5 (return pthang) (0.1...1.2) ((-0.5)...0.9) (0.5...2.5) (1.0...3.0) (0.1...4.0)
-- looft2 f r0 r1 = looft (looft f r0) r1
looft2 f r0 = looft (looft f r0)
-- looft3 f r0 r1 r2 = looft (looft (looft f r0) r1) r2
-- looft3 f r0 r1 = looft (looft (looft f r0) r1)
looft3 f r0 r1 = looft (looft2 f r0 r1)
looft4 f r0 r1 r2 = looft (looft3 f r0 r1 r2)
looft5 f r0 r1 r2 r3 = looft (looft4 f r0 r1 r2 r3)
lpthang' = do
  io <- lpthang
  io

-- Not sure this is better since it doesn't handle plain ranges
-- TODO: implement this by composition
lift5 :: (Rand a E, Rand b E, Rand c E, Rand d E, Rand e E) => (E -> E -> E -> E -> E -> IO z) -> (a -> b -> c -> d -> e -> (IO (IO z)))
lift5 f a b c d e = f $. a *. b *. c *. d *. e

rpthang :: IO Shape
rpthang = do
  -- ioshape <- pthang $.. (0.1, 1.2) *.. (-0.5, 0.9) *.. (0.5, 2.5) *.. (1.0, 3.0) *.. (0.1, 4.0)
  ioshape <- (lift5 pthang) (Range (0.1, 1.2)) (Range (-0.5, 0.9)) (Range (0.5, 2.5)) (Range (1.0, 3.0)) (Range (0.1, 4.0))
  shape <- ioshape
  return shape
-- thang = pthang 0.5 (-0.35) 2.0 1.5 0.2

infixl 4 $.
($.) :: Rand r E => (E -> b) -> r -> IO b
f $. rando = do
  e <- getE rando
  return (f e)

infixl 4 *.
(*.) :: Rand r E => IO (E -> b) -> r -> IO b
iof *. rando = do
  f <- iof
  e <- getE rando
  return (f e)

infixl 4 $..
($..) :: (E -> b) -> (Double, Double) -> IO b
f $.. pr = f $. Range pr

infixl 4 *..
(*..) :: IO (E -> b) -> (Double, Double) -> IO b
iof *.. pr = iof *. Range pr

class Rand r a where
  getE :: r -> IO a

data Range = Range (Double, Double)
instance Rand Range E where
  getE (Range (a, b)) = do
    n <- getStdRandom (randomR (a, b))
    return $ KF n

-- rpthang :: IO (E -> E -> E -> E -> IO Shape)
-- rpthang = luft pthang (0.1 :: Double, 1.2 :: Double)

-- luft :: Rando r => (E -> b) -> r -> IO b
-- luft f rando = do
--   a <- getE rando
--   let b = f a
--   return b

-- class Rando r where
--   getE :: r -> IO E

-- -- instance Random E

-- -- instance Random n => Rando (n, n) n where
-- --   get pr = getStdRandom (randomR pr)

-- -- instance Rando (Double, Double) where
-- instance (Random n, Fractional n, Real n) => Rando (n, n) where
--   getE pr = do
--     n <- getStdRandom (randomR pr)
--     return $ KF $ realToFrac n

-- -- instance Random n => Rando (n, n) E where
-- --   get pr = do
-- --     n <- getStdRandom (randomR pr)
-- --     return $ KF n

thang :: IO Shape
thang = pthang 0.5 (-0.35) 2.0 1.5 0.2
-- thang = do
--   rs0 <- randomShape
--   rs1 <- randomShape
--   let rs0' = rotation (osc 0.5) (pfGrid 2 2 rs0)
--   let rs1' = rotation (osc (-0.35)) (pfGrid 1.5 1.5 rs1)
--   let p = interp (osc 0.2) rs0' rs1'
--   return $ scale 0.1 p

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
