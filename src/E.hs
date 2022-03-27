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
) where

data Uniform = UF String
  deriving (Eq, Show, Read, Ord)

-- DSL for GLSL expressions
data E = KF Double | U Uniform | Add E E | Sub E E | Mul E E | Div E E | Length E | V2 E E | V3 E E E | V4 E E E E | XY | Sh E | ShRef Int
       | Abs E | Min E E | Max E E | X E | Y E | Neg E | Fun1 String Ty Ty E | Fun2 String Ty Ty Ty E E | Fun String [Ty] Ty [E] | Mat2 [E]
       | Comparison String E E | Cond E E E -- | V String
       | RGB E | A E
  deriving (Eq, Show, Read, Ord)

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
  deriving (Eq, Show, Read, Ord)

type Shape = Transform -> E
type UnOp = Shape -> Shape
type BinOp = Shape -> Shape -> Shape

data Transform = Transform E E
type Transformer = Transform -> Transform

time :: E
time = (U (UF "time"))
