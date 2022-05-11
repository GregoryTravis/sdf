{-# LANGUAGE DeriveGeneric  #-}

module E
( E(..)
, Ty(..)
, Uniform(..)
, (==.)
, (<.)
, (>.)
, (<=.)
, (>=.)
, Transform(..)
, Transformer
, Shape
, UnOp
, BinOp
, time
, snTest
) where

import Control.DeepSeq
import Data.HashMap.Strict
import Data.Hashable
import GHC.Generics (Generic, Generic1)
import System.IO.Unsafe
import System.Mem.StableName
import System.Random hiding (Uniform)
import System.Random.Stateful hiding (Uniform)
import qualified System.Random as SR
import qualified System.Random.Stateful as SRS

import Util hiding (time)

data Uniform = UF String
  deriving (Eq, Show, Read, Ord, Generic)
instance NFData Uniform

-- DSL for GLSL expressions
data E = KF Double | U Uniform | Add E E | Sub E E | Mul E E | Div E E | Length E | V2 E E | V3 E E E | V4 E E E E | XY | Sh E | ShRef Int
       | Abs E | Min E E | Max E E | X E | Y E | Neg E | Fun1 String Ty Ty E | Fun2 String Ty Ty Ty E E | Fun String [Ty] Ty [E] | Mat2 [E]
       | Comparison String E E | Cond E E E -- | V String
       | RGB E | A E
  deriving (Eq, Show, Read, Ord, Generic)
instance NFData E

-- -- DSL for GLSL programs
-- data GLSL = GLSL [UniformDecl] [Func]
-- data UniformDecl = UniformDecl Ty String
-- -- Env is the arg declarations
-- data Func = Func String Env Ty [Stmt]
-- data VarDecl = VarDecl String Ty
-- type Env = [VarDecl]
-- data Stmt = Decl VarDecl E | Assign E E | Return E

instance Num E where
  (+) = Add
  (*) = Mul
  abs = Abs
  signum = error "signum not implemented"
  fromInteger i = KF (fromInteger i)
  negate = Neg

-- fromRational, (recip | (/))
instance Fractional E where
  fromRational i = KF (fromRational i)
  (/) = Div

-- Not sure why I thought I needed this
-- instance Floating E

instance Random E

instance UniformRange E where
  uniformRM (KF lo, KF hi) g = KF <$> uniformRM (lo, hi) g
  uniformRM (Neg (KF lo), KF hi) g = KF <$> uniformRM (-lo, hi) g
  uniformRM (KF lo, Neg (KF hi)) g = KF <$> uniformRM (lo, -hi) g
  uniformRM (Neg (KF lo), Neg (KF hi)) g = KF <$> uniformRM (-lo, -hi) g
  uniformRM pr _ = error $ "UniformRange not implemented for: " ++ show pr

instance SRS.Uniform E
  where
    -- This worked but seems unnecessary
    -- uniformM g = KF <$> uniformRM (0.0, 1.0) g
    uniformM g = uniformRM (0.0, 1.0) g

infix 4 ==.
(==.) = Comparison "=="
infix 4 <.
(<.) = Comparison "<"
infix 4 >.
(>.) = Comparison ">"
infix 4 <=.
(<=.) = Comparison "<="
infix 4 >=.
(>=.) = Comparison ">="

data Ty = TF | TV2 | TV3 | TV4 | TM2 | TB
  deriving (Eq, Show, Read, Ord, Generic)
instance NFData Ty

type Shape = Transform -> E
type UnOp = Shape -> Shape
type BinOp = Shape -> Shape -> Shape

-- (Transform xy t)
data Transform = Transform E E
type Transformer = Transform -> Transform

time :: E
time = (U (UF "time"))

addy :: E -> E -> E
addy a b = a + b

instance Show (StableName a) where
  show sn = show (hashStableName sn)

-- instance Hashable (StableName a) where
--   hash = hashStableName

mksn :: a -> (a, StableName a)
mksn x = (x, sn)
  where sn = unsafePerformIO $ makeStableName x

snTest :: IO ()
snTest = do
  let n = KF 12.3
      sum = addy n n
  msp sum
  sn0 <- makeStableName sum
  sn1 <- makeStableName sum
  let sn2 = mksn sum
  let sn3 = mksn sum
  msp ("sns", sn0, sn1, sn2, sn3)
  msp $ sn0 == sn1
  let m0 = empty
      m1 = insert sn0 sum m0
  msp m1
  msp $ m1 !? sn1
  msp "hi snTest"
