module Lib where

import E

ssqrt = sh . Fun1 "sqrt" TF TF
ssin = sh . Fun1 "sin" TF TF
scos = sh . Fun1 "cos" TF TF
satan y x = sh $ Fun2 "atan" TF TF TF y x

sabs = sh . Abs
smod x y = sh $ Fun2 "mod" TF TF TF x y
sfloor = sh . Fun1 "floor" TF TF

sdFdx = sh . Fun1 "dFdx" TF TF
sdFdy = sh . Fun1 "dFdy" TF TF

smin x y = sh $ Fun2 "min" TF TF TF x y
smax x y = sh $ Fun2 "max" TF TF TF x y

smoothstep edge0 edge1 x = sh $ Fun "smoothstep" [TF, TF, TF] TF [edge0, edge1, x]

-- types are for alpha blending
mix x y a = sh $ Fun "mix" [TV3, TV3, TF] TV3 [x, y, a]
-- specific to (rgb, a)
vec4 rgb a = sh $ Fun "vec4" [TV3, TF] TV4 [rgb, a]
