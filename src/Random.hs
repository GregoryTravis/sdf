{-# LANGUAGE FlexibleInstances, FlexibleContexts, FunctionalDependencies , MultiParamTypeClasses, TypeOperators #-}

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

randomPrim :: IO Shape
-- randomPrim = randFromList allPrims
randomPrim = toE (map return allPrims :: [IO Shape])
randomPrim'' = map return allPrims :: [IO Shape]

-- randomUnOp :: IO UnOp
-- randomUnOp = randIO unOps

-- randomBinOp :: IO BinOp
-- randomBinOp = randIO binOps

unOps :: [IO UnOp]
unOps = [
    -- randomize1 scale scalers
    looft (return scale) [punk, pink]
  -- , randomize1 translation translators
  , translators'
  -- , randomize1 rotation rotators
  , rotators'
  -- , randomize2 grid gridders gridders
  , gridders'
  -- , randomize2 pfGrid gridders gridders
  , pfgridders'
  ]

randomUnOp' :: IO UnOp
randomUnOp' = toE [
    looft (return scale) [punk, pink]
  , translators'
  , rotators'
  , gridders'
  , pfgridders'
  ]

randomUnOp'' = [
    looft (return scale) [punk, pink]
  , translators'
  , rotators'
  , gridders'
  , pfgridders'
  ]

binOps :: [IO BinOp]
binOps = (map return allBinOps) ++
  [ interp <$> (KF <$> (getStdRandom (randomR (0.0, 0.1))))
  ]
randomBinOp' :: IO BinOp
randomBinOp' = toE ([looft (return interp) (0.0...0.1)] ++ (map return allBinOps))
randomBinOp'' = ([looft (return interp) (0.0...0.1)] ++ (map return allBinOps))

scalers :: [IO E]
-- scalers = error "asdf"
scalers = [
    KF <$> (getStdRandom (randomR (0.25, 4.0)))
  , osc <$> KF <$> (getStdRandom (randomR (2.0, 8.0)))
  ]

punk :: IO E
punk = KF <$> (getStdRandom (randomR (0.25, 4.0)))
punk' :: IO E
-- punk' = looft (return id) (0.25...4.0)
punk' = toE (0.25...4.0)
pink :: IO E
-- pink = osc <$> KF <$> (getStdRandom (randomR (2.0, 8.0)))
pink = looft (return osc) (2.0...8.0)
scalers' :: IO E
scalers' = toE [punk, pink]
-- randomize1 scale scalers :: IO UnOp

translators :: [IO E]
translators = [
    V2 <$> (KF <$> small) <*> (KF <$> small)
  , V2 <$> (KF <$> small) <*> (return $ KF 0.0)
  , V2 <$> (return $ KF 0.0) <*> (KF <$> small)
  ]
  where small :: IO Double

        small = getStdRandom (randomR ((-3.0), 3.0))
translators' :: IO UnOp
translators' = looft (return translation)
  [ (looft2 (return V2) small small)
  , (looft2 (return V2) small (rk (KF 0.0)))
  , (looft2 (return V2) (rk (KF 0.0)) small)
  ]
  where small = ((-3.0)...3.0)

rotators :: [IO E]
rotators = [
    KF <$> getStdRandom (randomR (0, pi))
  ]
rotators' :: IO UnOp
rotators' = looft (return rotation) (0.0...pi)

gridders :: [IO E]
gridders = [
    KF <$> dim
  ]
  where dim = getStdRandom (randomR (1.5, 3.0))
gridders' :: IO UnOp
gridders' = looft2 (return grid) dim dim
  where dim = (1.5...3.0)
pfgridders' :: IO UnOp
pfgridders' = looft2 (return pfGrid) dim dim
  where dim = (1.5...3.0)

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
randomShape = toE randomShapes
randomShapes :: [IO Shape]
randomShapes = [
    toE randomPrim''
  -- , randomUnOp' <*> randomPrim
  , toE randomUnOp'' <*> randomPrim
  -- , randomBinOp' <*> randomPrim <*> randomPrim
  , toE randomBinOp'' <*> randomPrim <*> randomPrim
  ]

-- lark :: IO Shape
-- lark = (toE randomUnOp'' <*>) randomPrim -- (return circle)

recipe :: IO Shape
recipe = randIO recipes
  where recipes = [
            lpthang'
            -- lpthangAllRand
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
  determinisitic
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

pthang :: E -> E -> E -> E -> E -> IO Shape
pthang r0 r1 g0 g1 interpRate = do
  rs0 <- randomShape
  rs1 <- randomShape
  let rs0' = rotation (osc r0) (pfGrid g0 g0 rs0)
  let rs1' = rotation (osc r1) (pfGrid g1 g1 rs1)
  let p = interp (osc interpRate) rs0' rs1'
  return $ scale 0.1 p

randIO :: [IO a] -> IO a
randIO ios = do
  io <- randFromList ios
  io

class Rend r a | r -> a where
  toE :: r -> IO a

-- TODO rename toE
instance Rend Range E where
  toE (Range (lo, hi)) = do
    n <- getStdRandom (randomR (lo, hi))
    return $ KF n

instance Rend [IO a] a where
  toE xs = do
    io <- randFromList xs
    io

data ConstantRandom a = ConstantRandom a
instance Rend (ConstantRandom a) a where
  toE (ConstantRandom a) = return a
rk = ConstantRandom

-- instance Rend (Double, Double) E where
--   toE (lo, hi) = do
--     n <- getStdRandom (randomR (lo, hi))
--     return $ KF n

-- instance (Random n, Fractional n, Real n) => Rend (n, n) E where
--   toE (lo, hi) = do
--     n <- getStdRandom (randomR (lo, hi))
--     return $ KF $ realToFrac n

looft :: Rend r a => IO (a -> b) -> r -> IO b
looft iof rend = do
  e <- toE rend
  f <- iof
  return $ f e

-- data Range = Range Double Double
(...) :: Double -> Double -> Range
a ... b = Range (a, b)

sspthang :: Shape -> Shape -> E -> E -> E -> E -> E -> IO Shape
sspthang rs0 rs1 r0 r1 g0 g1 interpRate = do
  let rs0' = rotation (osc r0) (pfGrid g0 g0 rs0)
  let rs1' = rotation (osc r1) (pfGrid g1 g1 rs1)
  let p = interp (osc interpRate) rs0' rs1'
  return $ scale 0.1 p

lpthangAllRand = join $ looft7 (return sspthang) randomShapes randomShapes (0.1...1.2) ((-0.5)...0.9) (0.5...2.5) (1.0...3.0) (0.1...4.0)

-- lpthang = looft (looft (return pthang) (0.1...1.2)) ((-0.5)...0.9)
lpthang = looft5 (return pthang) (0.1...1.2) ((-0.5)...0.9) (0.5...2.5) (1.0...3.0) (0.1...4.0)
looft2 :: (Rend r0 a, Rend r1 b) => IO (a -> b -> c) -> r0 -> r1 -> IO c
looft2 = (looft .) . looft
looft3 :: (Rend r0 a, Rend r1 b, Rend r2 c) => IO (a -> b -> c -> d) -> r0 -> r1 -> r2 -> IO d
looft3 = ((looft .) .) . looft2
looft4 :: (Rend r0 a, Rend r1 b, Rend r2 c, Rend r3 d) => IO (a -> b -> c -> d -> e) -> r0 -> r1 -> r2 -> r3 -> IO e
looft4 = (((looft .) .) .) . looft3
looft5 :: (Rend r0 a, Rend r1 b, Rend r2 c, Rend r3 d, Rend r4 e) => IO (a -> b -> c -> d -> e -> f) -> r0 -> r1 -> r2 -> r3 -> r4 -> IO f
looft5 = ((((looft .) .) .) .) . looft4
looft6 :: (Rend r0 a, Rend r1 b, Rend r2 c, Rend r3 d, Rend r4 e, Rend r5 f) => IO (a -> b -> c -> d -> e -> f -> g) -> r0 -> r1 -> r2 -> r3 -> r4 -> r5 -> IO g
looft6 = (((((looft .) .) .) .) .) . looft5
looft7 :: (Rend r0 a, Rend r1 b, Rend r2 c, Rend r3 d, Rend r4 e, Rend r5 f, Rend r6 g) => IO (a -> b -> c -> d -> e -> f -> g -> h) -> r0 -> r1 -> r2 -> r3 -> r4 -> r5 -> r6 -> IO h
looft7 = ((((((looft .) .) .) .) .) .) . looft6
lpthang' = join lpthang

-- -- Not sure this is better since it doesn't handle plain ranges
-- -- TODO: implement this by composition
-- lift5 :: (Rand a E, Rand b E, Rand c E, Rand d E, Rand e E) => (E -> E -> E -> E -> E -> IO z) -> (a -> b -> c -> d -> e -> (IO (IO z)))
-- lift5 f a b c d e = f $. a *. b *. c *. d *. e

-- rpthang :: IO Shape
-- rpthang = do
--   -- ioshape <- pthang $.. (0.1, 1.2) *.. (-0.5, 0.9) *.. (0.5, 2.5) *.. (1.0, 3.0) *.. (0.1, 4.0)
--   ioshape <- (lift5 pthang) (Range (0.1, 1.2)) (Range (-0.5, 0.9)) (Range (0.5, 2.5)) (Range (1.0, 3.0)) (Range (0.1, 4.0))
--   shape <- ioshape
--   return shape
-- -- thang = pthang 0.5 (-0.35) 2.0 1.5 0.2

-- infixl 4 $.
-- ($.) :: Rand r E => (E -> b) -> r -> IO b
-- f $. rando = do
--   e <- getE rando
--   return (f e)

-- infixl 4 *.
-- (*.) :: Rand r E => IO (E -> b) -> r -> IO b
-- iof *. rando = do
--   f <- iof
--   e <- getE rando
--   return (f e)

-- infixl 4 $..
-- ($..) :: (E -> b) -> (Double, Double) -> IO b
-- f $.. pr = f $. Range pr

-- infixl 4 *..
-- (*..) :: IO (E -> b) -> (Double, Double) -> IO b
-- iof *.. pr = iof *. Range pr

-- class Rand r a where
--   getE :: r -> IO a

data Range = Range (Double, Double)
-- instance Rand Range E where
--   getE (Range (a, b)) = do
--     n <- getStdRandom (randomR (a, b))
--     return $ KF n

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
