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
, chunky
, bubbles
, filaoaBub
, artifactBub
, graph
, bsp
, lcd
, dropshadow
, potdC
, modgriddy
, rainbowy
, nutsoRainbow
, potd4
, newpotdC
, someCircles
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
import DropShadows
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
  [ --("sizes", sizes <$> via KF <*> via KF <*> via KF <*> primPick)
  -- , ("filaoa", pure $ crecipe $ return $ evalShape filaoa)
    ("filaoa", pure $ return $ bandy black white $ evalShape filaoa)
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
sizes :: E Float -> E Float -> E Float -> Shape -> Shape
sizes rotvel xvel yvel s = trans shapes
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

chunky :: IO Color
chunky =
  let d = 0.5 + (0.6 * (ssin (time / KF 2.0))) -- 0.8
      l = translation (V2 (-d) 0.0) circle
      r = translation (V2 d 0.0) circle
      all = l `smoothUnion` r
   in (return . bubble . evalShape) (transform chunkify all)

chunkify :: Transformer
chunkify (Transform uv t) =
  let uvChunkSize = 0.05
      u = _x uv
      v = _y uv
      u' = uvChunkSize * (sfloor (u / uvChunkSize))
      v' = uvChunkSize * (sfloor (v / uvChunkSize))
   in Transform (V2 u' v') t

cone :: E Float -> Color
cone = colorGrad white black (-1.0) 0.0

aCircle :: IO Color
--aCircle = (return . smooth white black . evalShape) circle -- $ flower 4.0
aCircle = (return . iqBandy . evalShape) circle -- $ flower 4.0

bubbles :: IO Color
bubbles =
  let d = 0.5 + (0.6 * (ssin (time / KF 2.0))) -- 0.8
      l = translation (V2 (-d) 0.0) circle
      r = translation (V2 d 0.0) circle
      all = l `smoothUnion` r
  in (return . bubble . evalShape) all

graph :: IO Color
graph = graphit (\x -> x * x)

graphit :: (E Float -> E Float) -> IO Color
graphit fn =
  -- unit aquare is -1..1 in both axes
  let x = _x uv
      y = _y uv
      above = red
      below = green
   in return $ Cond (fn x <. y) below above

up :: E (V3 Float)
up = V3 0.0 0.0 1.0

-- bubbleNorm :: E Float -> E Float -> E (V3 Float)
-- bubbleNorm radius dist =
--   let duv = sh $ calcduv dist
--       vertLen = sh $ ssqrt (KF (-2.0) * dist * radius - (dist * dist))
--       vert = vertLen *^ up
--       curveNorm = vec3 ((dist + radius) *^ duv) +^ vert
--       isInCurve = (dist <=. KF 0.0) &&. (dist >. (-radius))
--    in norm3 $ Cond isInCurve curveNorm up

bubbleHeight :: E Float -> E Float -> E Float
bubbleHeight radius dist =
  -- TODO or faster: ssqrt (-(dist * (dist + (KF 2.0 * radius))))
  let d = dist - 0.0 -- change to e.g. 0.1 and jaggies go away
      -- smax because might go subpixel above 0 resulting in imaginary height (nan)
      curveHeight = ssqrt (smax 0 (KF (-2.0) * d * radius - (d * d)))
      topHeight = radius
   in Cond (d >=. (-radius)) curveHeight topHeight

-- Bad edge
bubbleNorm2 :: E Float -> E Float -> E (V3 Float)
bubbleNorm2 radius dist =
  let height = sh $ bubbleHeight radius dist
      dHeightDx = sdFdx height
      dHeightDy = sdFdy height
      du = sh $ sdFdx (_x uv)
      dv = sh $ sdFdy (_y uv)
   in norm3 $ V3 (-(dHeightDx * dv)) (-(dHeightDy * du)) (du * dv)

lightNorm :: E (V3 Float) -> E Float
lightNorm norm =
  -- TODO opt don't do these every time?
  let  lightDir = norm3 $ V3 (-1.0) 1.0 1.0
   in norm `dot3` lightDir

-- TODO opt add sh
bubbleInside :: E Float -> Color
bubbleInside dist =
  let dark = bw3 (KF 0.0)
      bright = bw3 (KF 1.0)
      norm = bubbleNorm2 0.5 dist
      curveColor = colorGrad dark bright (-1.0) 1.0 (lightNorm norm)
   in curveColor

bubble :: E Float -> Color
bubble dist =
  -- TODO conditional to avoid bubble everywhere?
  let outsideColor = green
   in smooth (bubbleInside dist) outsideColor dist
   --in colorGrad red green (KF (-1.0)) (KF 1.0) (sdFdx dist * 1000.0)
   --in Cond (dist >. (-0.99)) green red

artifactBub :: IO Color
artifactBub = (return . bubble . evalShape) artifact

artifact :: Shape
artifact =
  let tm = KF 19.1
   in scale (KF 0.1) $ smoothUnion (scale (tm /^ (KF 10.0)) filaoa') (rotation (tm /^ (KF 10.0)) filaoa')
  where filaoa' = pfGrid (KF 2.25) (KF 2.25) circle

filaoaBub :: IO Color
--filaoaBub = (return . smooth white green . evalShape) filaoa
filaoaBub = (return . bubble . evalShape) filaoa

ruler :: Color -> Color -> E Float -> E Float -> Color
ruler c0 c1 unitSize x =
  let lower = smod x (unitSize * KF 2.0) <. unitSize
   in Cond lower c0 c1

basicGrid :: Color -> Color -> E Float -> E Float -> E Float -> Color
basicGrid c0 c1 unitSize x y =
  let lowerX = smod x (unitSize * KF 2.0) <. unitSize
      lowerY = smod y (unitSize * KF 2.0) <. unitSize
   in Cond (lowerX /=. lowerY) c0 c1

calibrator :: E Float -> E Float -> E Float -> Color
calibrator unitSize x y =
  let d = bw3 (KF 0.1)
      l = bw3 (KF 0.9)
      us10 = unitSize / 10.0
      us100 = unitSize / 100.0
      coarse = basicGrid d l us10 x y
      fine = basicGrid d l us100 x y
      gridd = colorGrad coarse fine 0.0 unitSize 0.5
      xgrad = colorGrad red green 0.0 unitSize x
      ygrad = colorGrad blue yellow 0.0 unitSize y
      grad = colorGrad xgrad ygrad 0.0 unitSize 0.5
   in gridd + grad

-- Normalized gradient vector, accounting for scale.
calcduv :: E Float -> E (V2 Float)
calcduv dist =
  let dDistX = sdFdx dist
      dDistY = sdFdy dist
      dDistXY = V2 dDistX dDistY
      dDist = Length dDistXY
   in dDistXY /^ dDist

bsp :: IO Color
bsp =
  let px = halfSpace
      rd = 0.2
      l = translation (V2 (-rd) 0.0) $ rotation (KF pi) px
      r = translation (V2 rd 0.0) px
      t = translation (V2 0.0 (-rd)) $ rotation (KF (pi/2)) px
      b = translation (V2 0.0 rd) $ rotation (KF (-(pi/2))) px
      square = intersections [l, r, t, b]
      all = difference (rotation time square) (rotation (-(time / 2)) square)
  in (return . smooth white black . evalShape) all

splitScreenHor :: (E Float -> Color) -> (E Float -> Color) -> (E Float -> Color)
splitScreenHor colorer0 colorer1 dist =
  Cond (_x uv >. 0) (colorer0 dist) (colorer1 dist)

splitScreenVer :: (E Float -> Color) -> (E Float -> Color) -> (E Float -> Color)
splitScreenVer colorer0 colorer1 dist =
  Cond (_y uv >. 0) (colorer0 dist) (colorer1 dist)

lcds :: [(Color, Color)]
lcds = [
  -- https://www.reddit.com/r/embedded/comments/1ikux0v/where_to_find_big_and_finer_resolution_monochrome/
  (mkCol3_256 84 91 66, mkCol3_256 134 148 113),
  -- https://forum.arduino.cc/t/large-backlight-monochrome-lcd/206007/3
  (mkCol3_256 35 39 32, mkCol3_256 137 152 147),
  -- https://huaxianjing.com/16x2-monochrome-character-lcd-module/
  (mkCol3_256 65 88 22, mkCol3_256 219 231 75),
  -- https://www.adafruit.com/product/772
  (mkCol3_256 163 194 197, mkCol3_256 50 113 245)
  ]

lcd :: IO Color
lcd =
  let (fg, bg) = lcds !! 3
      horc0 = splitScreenHor (smpr $ lcds !! 0) (smpr $ lcds !! 1)
      horc1 = splitScreenHor (smpr $ lcds !! 2) (smpr $ lcds !! 3)
      colorer = splitScreenVer horc0 horc1
      all = filaoa
  in (return . colorer . evalShape) all
  where smpr (fg, bg) = smooth fg bg

-- -- r: pusher, e: pushee, s: smooshed pushee
-- -- Adjusting e based on r; r is left alone
-- -- r < 1.1 => s > 0
-- -- 1.1 < r < 1.2 => 0 < s < e_e
-- -- 1.2 < r => s = e
-- -- where e_e is the distance where r is 1.2
-- smoosh :: Shape -> Shape -> Shape
-- smoosh pusher pushee t@(Transform uv _) =
--   let r = transform pusher t
--       e = transform pushee t
--       e_e = e * 0.8
--       d | r < 1.1 = 0.1

dropshadow :: IO Color
dropshadow =
  let s = scale 0.5 square
      dist = 0.05
      dir = V2 dist (-dist)
      (fg, bg) = lcds !! 2
      --bgShadow = V4 0.5 0.5 0.5 1.0
      bgShadow = mix4 white fg 0.8
      color = dropShadow dir fg bg bgShadow s idTransform
   in return color

rainbowShape :: Shape
rainbowShape tr@(Transform xy _) =
  let circ = circle tr
      legs = (sabs $ _x xy) - 1
      both = Cond (_y xy >=. 0) circ legs
   in both

rainbowy :: IO Color
rainbowy =
  let all = rainbowShape
      --colors = arr [red, white, blue, green, yellow]
      colors = randBands (0.2, 0.4, 0.75) 0.3 5
  in (return . bands colors black . evalShape) all

modgriddy :: IO Color
modgriddy =
  let shp :: E Float -> E Float -> Shape
      shp i j =
         scale 0.3 $ translation (V2 1 1) $ rotation (i * j * time) $ rainbowShape
      colorzer :: E Float -> E Float -> Transform -> (E Float -> Color)
      colorzer gi gj _ dist =
        let colorz = arr [V4 (aBand 0.1 n gi) (aBand 0.2 n gj) (aBand 0.3 n (gi+gj)) 1.0 | n <- take 5 [0..]]
         in bands colorz black dist
        where aBand r n x = smod (x * r * KF n) 1.0
      pic :: Picture
      pic = scale 0.2 (modgrid 1 1 colorzer <*> modgrid 1 1 shp)
   in return $ evalShape pic

nutsoRainbow :: E Float -> IO Color
nutsoRainbow scail =
  let shp i j =
        let ai = i / 10
            aj = j / 10
            -- zany
            -- ai' = i / 10
            -- aj' = j / 10
            -- mx = _x mouse
            -- my = _y mouse
            -- ai = mix1 mx ai' aj'
            -- aj = mix1 my ai' aj'
         in scale 0.3 $ translation (V2 1 1) $ rotation (i * j * time) $ rainbowShape
      colors = randBands (0.2, 0.4, 0.75) 0.3 5
      beej = arr [V4 (_x uv) (_y uv) 0.0 1.0]
      -- colorz = arr [V4 (smod (_x uv * 1.0 * KF n) 1.0) (smod (_y uv * 1.0 * KF n) 1.0) 0.2 1.0 | n <- take 5 [0..]]
      colorzer :: E Float -> E Float -> Transform -> (E Float -> Color)
      colorzer gi gj (Transform xy _) dist =
        let colorz = arr [V4 (smod (gi * 0.1 * KF n) 1.0) (smod (gj * 0.2 * KF n) 1.0) (smod ((gi + gj) * 0.3 * KF n) 1.0) 1.0 | n <- take 5 [0..]]
         in bands colorz black dist
      mgcolorzer :: Transform -> (E Float -> Color)
      mgcolorzer = scale scail $ modgrid 1 1 colorzer
      evaled_mgcolorzer :: E Float -> Color
      evaled_mgcolorzer = mgcolorzer idTransform
      all = scale scail $ modgrid 1 1 shp
      -- all = scale 0.1 $ grid 1 1 circle
  -- in (return . bands colorz (beej !!. KI 0) . evalShape) all
  in (return . evaled_mgcolorzer . evalShape) all

cool20251221_2 :: IO Color
cool20251221_2 =
  let shp i j =
        let ai = i / 10
            aj = j / 10
            -- zany
            -- ai' = i / 10
            -- aj' = j / 10
            -- mx = _x mouse
            -- my = _y mouse
            -- ai = mix1 mx ai' aj'
            -- aj = mix1 my ai' aj'
         in scale 0.3 $ translation (V2 1 1) $ rotation (i * j * time) $ rainbowShape
      colors = randBands (0.2, 0.4, 0.75) 0.3 5
      beej = arr [V4 (_x uv) (_y uv) 0.0 1.0]
      colorz = arr [V4 (smod (_x uv * 1.0 * KF n) 1.0) (smod (_y uv * 1.5 * KF n) 1.0) 0.2 1.0 | n <- take 5 [0..]]
      all = scale 0.2 $ modgrid 1 1 shp
      -- all = scale 0.1 $ grid 1 1 circle
  in (return . bands colorz (beej !!. KI 0) . evalShape) all

cool20251221 :: IO Color
cool20251221 =
  let shp i j =
        let ai = i / 10
            aj = j / 10
            -- zany
            -- ai' = i / 10
            -- aj' = j / 10
            -- mx = _x mouse
            -- my = _y mouse
            -- ai = mix1 mx ai' aj'
            -- aj = mix1 my ai' aj'
         in scale 0.3 $ translation (V2 1 1) $ rotation (i * j * time) $ rainbowShape
      colors = randBands (0.2, 0.4, 0.75) 0.3 5
      all = scale 0.2 $ modgrid 1 1 shp
      -- all = scale 0.1 $ grid 1 1 circle
  in (return . bands colors black . evalShape) all

sineMovement =
 let wave = ssin time
     disp = 1.7
     base = circle
     moving = translation (V2 (disp * wave) 0) $ scale 0.1 base
  in (base, moving)

mouseMovement =
  let wave = ssin time
      disp = 1.7
      big = 1
      small = 0.1
      base = scale big circle
      moving = translation (V2 (_x mouse) (-(_y mouse))) $ scale 1.0 (scale small circle) -- cir
  in (base, moving)

smoosh :: Shape -> Shape -> Shape
smoosh pusher pushee t@(Transform uv _) =
  let r = evalShape pusher
      e = evalShape pushee
      rlo = 0.1
      rhi = 2.7
      d = mix1 0.1 e (smoothstep rlo rhi r)
   in d

potd :: Shape
potd =
  let (base, moving) = mouseMovement
      all = (smoosh moving base) `union` moving
   in all

potd4 :: IO Color
potd4 =
  let (base, moving) = mouseMovement
      moving2 = flipX moving
      moving3 = flipY moving
      moving4 = flipY moving2
      -- union before smooshing does less drippiness
      -- smooshes = smoosh movings base
      smooshes = smoosh moving4 (smoosh moving3 (smoosh moving2 (smoosh moving base)))
      movings = smoothUnions [moving, moving2, moving3, moving4]
      all = smooshes `union` movings
   in (return . smooth (bubbleInside (evalShape all)) black . evalShape) all

-- TODO sh potd
potdC :: IO Color
potdC = (return . smooth (bubbleInside (evalShape potd)) black . evalShape) potd

distScale :: E Float -> Shape -> Shape
distScale s shape transform = s * (shape transform)

newsmoosh :: Shape -> Shape -> Shape
newsmoosh pusher pushee t@(Transform uv _) =
  let r = evalShape pusher
      e = evalShape pushee
      rlo = 0.3
      rhi = 0.6
      inBand = mix1 0.2 e (smoothstep rlo rhi r)
      inside = 0.5
      outside = e
      dInsideE = Cond (r <. rlo)
               inside
               (Cond (r <. rhi)
                     inBand
                     outside)
      d = Cond (e <=. 0) dInsideE 0.5
   in d

newpotd :: Shape
newpotd =
  let wave = ssin time
      disp = 1.7
      big = 0.5
      small = 0.2
      base = scale big circle
      moving = translation (V2 (_x mouse) (-(_y mouse))) $ scale 1.0 (scale small circle) -- cir
      --moving = translation (V2 (disp * wave) 0) $ scale 0.1 base
      -- all = (smoosh moving base) `union` (distScale 0.9 moving)
      all = (newsmoosh moving base) `union` moving
      --all = smoothUnion moving base
   in all

newpotdC :: IO Color
-- potdC = (return . smooth white black . evalShape) potd
-- potdC = (return . r2g . evalShape) potd
newpotdC = (return . iqBandy . evalShape) newpotd

r2g :: E Float -> Color
r2g dist =
  Cond (dist <=. KF 0.0)
    (mix4 red green (dist + 1.0))
    black

bugLacyEdge :: Shape
bugLacyEdge =
  let wave = ssin time
      disp = 0.7
      base = circle
      c0 :: Shape
      c0 = translation (V2 (-disp) wave) base
      c1 = translation (V2 disp (-wave)) base
      pc :: Transform -> E Float
      pc tr =
        let c0d = c0 tr
            c1d = c1 tr
            insideBoth = c0d <. 0 &&. c1d <. 0
            -- if inside both, push c1 back by returning a bit more than 0 for it
         in Cond insideBoth (-1) c1d
      all = union c0 pc
   in scale 0.5 $ all

someCircles :: IO Color
someCircles = (return . bandy white black . evalShape . sizes 0.1 0.1 0.0) circle -- $ flower 4.0

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

randomUnOp :: Rnd (UnOp Dist)
randomUnOp = uniformM
  [ scale <$> scalers
  , translation <$> translators
  , rotation <$> rotators
  , gridder
  , pfGridder
  ]

randomBinOp :: Rnd (BinOp Dist)
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

gridder :: Rnd (UnOp Dist)
gridder = grid <$> dim <*> dim
  where dim = 1.5...3.0

pfGridder :: Rnd (UnOp Dist)
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
