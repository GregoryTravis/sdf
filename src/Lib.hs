module Lib where

import E

ssqrt = Sh . Fun1 "sqrt" TF TF
ssin = Sh . Fun1 "sin" TF TF
scos = Sh . Fun1 "cos" TF TF

sabs = Sh . Abs
smod x y = Sh $ Fun2 "mod" TF TF TF x y
sfloor = Sh . Fun1 "floor" TF TF
