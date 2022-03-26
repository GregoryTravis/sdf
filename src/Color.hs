module Color
() where

import E
import Lib
import Util

bevelWidth :: Double
bevelWidth = 0.075

bevelDistToHeight :: E -> E
bevelDistToHeight dist =
  Cond (dist <. KF (-bevelWidth))
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

black = V4 0.0 0.0 0.0 1.0
white = V4 1.0 1.0 1.0 1.0
gray = V4 0.5 0.5 0.5 1.0

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
