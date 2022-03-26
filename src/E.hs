module E
( E(..)
, Ty(..)
, Uniform(..)
, (==.)
, Transform(..)
, Transformer
, Shape
, UnOp
, BinOp
, time
) where

data Uniform = UF String
  deriving (Eq, Show, Read, Ord)

data E = KF Double | U Uniform | Add E E | Sub E E | Mul E E | Div E E | Length E | V2 E E | XY | Sh E | ShRef Int
       | Abs E | Min E E | Max E E | X E | Y E | Neg E | Fun1 String Ty Ty E | Fun2 String Ty Ty Ty E E | Mat2 [E]
       | Comparison String E E | Cond E E E
  deriving (Eq, Show, Read, Ord)

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

infix 4 ==.
(==.) = Comparison "=="

data Ty = TF | TV2 | TM2 | TB
  deriving (Eq, Show, Read, Ord)

type Shape = Transform -> E
type UnOp = Shape -> Shape
type BinOp = Shape -> Shape -> Shape

data Transform = Transform E E
type Transformer = Transform -> Transform

time :: E
time = (U (UF "yeah"))
