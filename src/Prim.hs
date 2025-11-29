{-# LANGUAGE GADTs #-}

module Prim
( circle
, square
, halfSpace
, flower
, allPrims ) where

import E
import Lib

square :: Shape
square (Transform xy _) =
  let center = sh $ V2 (0.0 :: E Float) 0.0
      radius = sh 1.0
      sd = sh $ sabs (xy -^ center)
      dist = sh $ (smax (_x sd) (_y sd) / radius) - 1.0
   in dist

circle :: Shape
circle (Transform xy _) =
  let dist = Length xy -^ KF 1.0
   in dist

-- Default half space: interior is negative x axis
halfSpace :: Shape
halfSpace (Transform xy _) = _x xy

-- E should be integral
flower :: E Float -> Shape
flower numPetals (Transform xy _) =
  let ang = sh $ satan (_y xy) (_x xy)
      raw_dist = sh $ ssqrt (Length xy)
      radius = sh $ liftedAmp
      amp = sabs $ ssin $ (ang * (numPetals / 2.0))
      liftedAmp = lift + (1.0 - lift) * amp
      lift = 0.2
      dist = sh $ raw_dist / radius
   in dist - 1.0

    -- let ang = non_stupid_atan2(x, y);
    -- let raw_dist = (x*x + y*y).sqrt();
    -- let radius = (ang * (self.num_petals as f32 / 2.0)).sin().abs();
    -- let dist = raw_dist / radius;
    -- // println!("flower {} {} {} {} {} {}", x, y, ang, raw_dist, radius, dist);
    -- dist - 1.0

allPrims :: [Shape]
allPrims = [circle, square]
