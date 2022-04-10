module Prim
( circle
, square
, flower
, allPrims ) where

import E
import Lib

square :: Shape
square (Transform xy _) =
  let center = Sh $ V2 0.0 0.0
      radius = Sh 1.0
      sd = Sh $ Abs (xy - center)
      dist = Sh $ (Max (X sd) (Y sd) / radius) - 1.0
   in dist

circle :: Shape
circle (Transform xy _) =
  let dist = Length xy - 1.0
   in dist

-- E should be integral
flower :: E -> Shape
flower numPetals (Transform xy t) =
  let ang = Sh $ satan (Y xy) (X xy)
      raw_dist = Sh $ ssqrt (Length xy)
      radius = Sh $ sabs $ ssin $ (ang * (numPetals / 2.0))
      dist = Sh $ raw_dist / radius
   in dist - 1.0

    -- let ang = non_stupid_atan2(x, y);
    -- let raw_dist = (x*x + y*y).sqrt();
    -- let radius = (ang * (self.num_petals as f32 / 2.0)).sin().abs();
    -- let dist = raw_dist / radius;
    -- // println!("flower {} {} {} {} {} {}", x, y, ang, raw_dist, radius, dist);
    -- dist - 1.0

allPrims :: [Shape]
allPrims = [circle, square]
