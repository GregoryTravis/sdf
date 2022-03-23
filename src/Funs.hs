module Funs
( osc ) where

import E
import Lib
import Util hiding (time)

-- Hz
osc :: E -> E
osc rate = Sh $ (ssin (time * rate) + 1) / 2
