module Lib where

import E

ssqrt = Sh . Fun1 "sqrt" TF TF
ssin = Sh . Fun1 "sin" TF TF
scos = Sh . Fun1 "cos" TF TF

sabs = Sh . Abs
smod x y = Sh $ Fun2 "mod" TF TF TF x y
sfloor = Sh . Fun1 "floor" TF TF

smoothstep edge0 edge1 x = Sh $ Fun "smoothstep" [TF, TF, TF] TF [edge0, edge1, x]
