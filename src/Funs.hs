module Funs
( osc ) where

import E
import Lib
import Util hiding (time)

-- Hz
osc :: E Float -> E Float -> E Float
osc t rate = sh $ (ssin (t * rate) + 1) / 2
