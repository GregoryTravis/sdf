module Funs
( osc ) where

import E
import Lib
import Util hiding (time)

-- Hz
osc :: E Float -> E Float
osc rate = sh $ (ssin (time * rate) + 1) / 2
