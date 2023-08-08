module Color where

import System.Random

import E
import Lib
import Util

type Color = E (V4 Float)

mkCol :: Float -> Float -> Float -> Float -> Color
mkCol r g b a = V4 (KF r) (KF g) (KF b) (KF a)

black = mkCol 0.0 0.0 0.0 1.0
white = mkCol 1.0 1.0 1.0 1.0
gray = mkCol 0.5 0.5 0.5 1.0
red = mkCol 1.0 0.0 0.0 1.0
green = mkCol 0.0 1.0 0.0 1.0
blue = mkCol 0.0 0.0 1.0 1.0
nothing = mkCol 0.0 0.0 0.0 0.0

randomColor :: IO Color
randomColor = do
  r <- getStdRandom (randomR (0.0, 1.0))
  g <- getStdRandom (randomR (0.0, 1.0))
  b <- getStdRandom (randomR (0.0, 1.0))
  return $ V4 (KF r) (KF g) (KF b) 1.0

randomTransparentColor :: IO Color
randomTransparentColor = do
  r <- getStdRandom (randomR (0.0, 1.0))
  g <- getStdRandom (randomR (0.0, 1.0))
  b <- getStdRandom (randomR (0.0, 1.0))
  a <- getStdRandom (randomR (0.0, 1.0))
  return $ V4 (KF r) (KF g) (KF b) (KF a)

-- likelihood 0..1
randomMaybeTransparentColor :: Double -> IO Color
randomMaybeTransparentColor transparencyLikelihood = do
  n <- getStdRandom (randomR (0.0, 1.0))
  let transparent = n < transparencyLikelihood
  if transparent
    then randomTransparentColor
    else randomColor

scaleAwareAA :: E Float -> E Float
scaleAwareAA dist =
  let dDistX = sdFdx dist
      dDistY = sdFdy dist
      dDistXY = V2 dDistX dDistY
      dDist = Length dDistXY
   in dDist

-- scale-aware anti-aliased edge
-- slow
smooth :: Color -> Color -> E Float -> Color
smooth fg bg dist =
  let smoothRadius = scaleAwareAA dist
      bwBlend = smoothstep (-smoothRadius) smoothRadius dist
      color = bwBlend *^ bg +^ (KF 1.0 -^ bwBlend) *^ fg;
   in sh color

bandy :: Color -> Color -> E Float -> Color
bandy fg bg dist =
  let smoothRadius = scaleAwareAA dist
      bwBlend1 = smoothstep (x-smoothRadius) (x+smoothRadius) dist
        where x = 0.0 * k
      bwBlend2 = smoothstep (x-smoothRadius) (x+smoothRadius) dist
        where x = (-0.1 * k)
      bwBlend3 = smoothstep (x-smoothRadius) (x+smoothRadius) dist
        where x = (-0.96 * k)
      bwBlend = bwBlend2
      k = 2.5
      color1 = bwBlend1 *^ bg +^ (KF 1.0 -^ bwBlend1) *^ red;
      color2 = bwBlend2 *^ color1 +^ (KF 1.0 -^ bwBlend2) *^ blue;
      color3 = bwBlend3 *^ color2 +^ (KF 1.0 -^ bwBlend3) *^ fg;
      color = color3
      -- color = bwBlend * bg + (1.0 - bwBlend) * fg;
      -- color' = bwBlend2 * color + (1.0 - bwBlend2) * red
   in sh color

-- This is bad beacuse the derivative (or the way I'm calculating it) is
-- imprecise, and so using it to scale-independently set the band edges causes a
-- weird feathering effect, but only if the bands aren't thin.
siBandy :: Color -> Color -> E Float -> Color
siBandy fg bg dist =
  let smoothRadius = scaleAwareAA dist
      bwBlend1 = smoothstep (x-1.0) (x+1.0) (dist / smoothRadius)
        where x = 0.0 / smoothRadius
      bwBlend2 = smoothstep (x-1.0) (x+1.0) (dist / smoothRadius)
        where x = -20.0
      bwBlend3 = smoothstep (x-1.0) (x+1.0) (dist / smoothRadius)
        where x = -40.0 -- / smoothRadius
      bwBlend = bwBlend2
      color1 = bwBlend1 *^ bg +^ (KF 1.0 -^ bwBlend1) *^ red;
      color2 = bwBlend2 *^ color1 +^ (KF 1.0 -^ bwBlend2) *^ blue;
      color3 = bwBlend3 *^ color2 +^ (KF 1.0 -^ bwBlend3) *^ fg;
      color = color3
      --pixelWidth = 1.0 / smin (_x (U (UF "resolution"))) (_y (U (UF "resolution")))
      resolution = Uniform "resolution" :: E (V2 Float)
      pixelWidth = 1.0 / smin (_x resolution) (_y resolution)
      -- color = bwBlend * bg + (1.0 - bwBlend) * fg;
      -- color' = bwBlend2 * color + (1.0 - bwBlend2) * red
   in sh color

-- anti-aliased edge
oldsmooth :: E Float -> E Float -> E Float -> E Float
oldsmooth fg bg dist =
  let smoothRadius = 0.03
      bwBlend = smoothstep (-smoothRadius) smoothRadius dist
      color = bwBlend *^ bg +^ (KF 1.0 -^ bwBlend) *^ fg;
   in sh color

alphaBlend :: Color -> Color -> Color
alphaBlend bg fg = sh $ vec4 (mix (rgb $ sh bg) (rgb $ sh fg) (_a fg)) 1.0

-- This is a fold
alphaBlends :: [Color] -> Color
alphaBlends es = sh $ alphaBlends' (black : es)
  where alphaBlends' [] = error "alphaBlends: impossible"
        alphaBlends' [e] = e
        alphaBlends' (bg : fg : es) = alphaBlends' (alphaBlend bg fg : es)

---- half-completed bevel edge, needs entire shader to be in E

bevelWidth :: Float
bevelWidth = 0.075

bevelDistToHeight :: E Float -> E Float
bevelDistToHeight dist =
  Cond (dist <. KF (negate bevelWidth))
       1.0
       (let sd = dist / KF bevelWidth
         in scos ((KF pi / 2.0) * (sd + 1.0)))

-- const BEVEL_WIDTH: f32 = 0.075;
-- fn bevel_dist_to_ht(d: f32) -> f32 {
--   let pi = std::f32::consts::PI;
--   if d < -BEVEL_WIDTH {
--     1.0
--   } else {
--     let sd = d / BEVEL_WIDTH;
--     ((pi/2.0) * (sd+1.0)).cos()
--   }
-- }

-- This should wait until the whole shader is written in E, because it needs
-- screen coords as well, and makes 5 calls to the shape

-- -- x -> y -> dist -> vec4 color
-- type Colorer = E -> E -> E -> E
-- bevel :: Colorer
-- bevel x y dist =
--   let bit = KF 0.005
--       ax = x - bit
--       ay = y
--       bx = x + bit
--       by = y
--       cx = x
--       cy = y - bit
--       dx = x
--       dy = y + bit

--       adist = shape(ax, ay, t)
--       bdist = shape(bx, by, t)
--       cdist = shape(cx, cy, t)
--       ddist = shape(dx, dy, t)

--       az = bevel_dist_to_ht(adist)
--       bz = bevel_dist_to_ht(bdist)
--       cz = bevel_dist_to_ht(cdist)
--       dz = bevel_dist_to_ht(ddist)

--       a = Point3::new(ax, ay, az)
--       b = Point3::new(bx, by, bz)
--       c = Point3::new(cx, cy, cz)
--       d = Point3::new(dx, dy, dz)

--       ba: Vector3<f32> = b - a
--       cd: Vector3<f32> = c - d

--       norm = cd.cross(&ba).normalize()
--       brightness = norm.dot(&light)

--       bev = GRAY.lerp(WHITE, brightness.clamp(0.0, 1.0))
--    in Cond (dist > 0.0) BLACK bev
