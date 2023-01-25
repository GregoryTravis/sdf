{-# LANGUAGE AllowAmbiguousTypes, EmptyDataDeriving, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, StandaloneDeriving #-}

module E
(   E(..)
  , sh
  -- , Ty(..)
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
  -- , time
  -- , snTest
  , GlslType
  , V2
  , V3
  , V4
  , Bul
  , Mat2
  , (+^)
  , (-^)
  , (*^)
  , (/^)
  , typeName
  , _x
  , _y
  , xy
  , yx
  , xyz
  , yxz
  , rgb
  , _a
  , Fielder(..)
  , swizzleFields
  , Dist
  , sabs
) where

import Control.DeepSeq
import Control.Monad (when)
import Data.HashMap.Strict
import Data.Hashable
-- import GHC.Generics (Generic, Generic1)
import System.IO.Unsafe
import System.Mem.StableName
import System.Random hiding (Uniform)
import System.Random.Stateful hiding (Uniform)
import qualified System.Random as SR
import qualified System.Random.Stateful as SRS

import Util hiding (time)

data Uniform = UF String
  deriving (Eq, Show, Read, Ord) -- , Generic)
-- NFData instance Uniform

-- Use DataKinds?
data V2 a
  deriving (Eq, Show)
data V3 a
  deriving (Eq, Show)
data V4 a
  deriving (Eq, Show)
data Mat2 a
  deriving (Eq, Show)
data Bul
  deriving (Eq, Show)

data E a where
  KF :: Float -> E Float
  KD :: Double -> E Double
  KI :: Int -> E Int
  V2 :: (Show a, GlslType a) => E a -> E a -> E (V2 a)
  V3 :: (Show a, GlslType a) => E a -> E a -> E a -> E (V3 a)
  V4 :: (Show a, GlslType a) => E a -> E a -> E a -> E a -> E (V4 a)
  Add :: (GlslType a, GlslType b, GlslType c, Promotable a b c) => E a -> E b -> E c
  Sub :: (GlslType a, GlslType b, GlslType c, Promotable a b c) => E a -> E b -> E c
  Mul :: (GlslType a, GlslType b, GlslType c, Promotable a b c) => E a -> E b -> E c
  Div :: (GlslType a, GlslType b, GlslType c, Promotable a b c) => E a -> E b -> E c
  Length :: (GlslType a, GlslType b, Lengthable a b) => E a -> E b
  Uniform :: GlslType a => String -> E a
  Swizzle :: (GlslType (vv n), GlslType (v n), SwizzlesTo (vv n) (v n)) => Swizzler (v n) -> E (vv n) -> E (v n)
  Field :: (GlslType (vv n), FieldsTo (vv n) n) => Fielder n -> E (vv n) -> E n
  Fun1 :: (GlslType a, GlslType b, Show a) => String -> E a -> E b
  Fun2 :: (Show a, Show b, GlslType a, GlslType b) => String -> E a -> E b -> E c
  Fun3 :: (Show a, Show b, Show c, GlslType a, GlslType b, GlslType c) => String -> E a -> E b -> E c -> E d
  Neg :: GlslType a => E a -> E a
  Mat2 :: (Show a, GlslType a) => [E a] -> E (Mat2 a)
  Comparison :: (Show a, GlslType a) => String -> E a -> E a -> E Bul
  Cond :: GlslType a => E Bul -> E a -> E a -> E a
  -- These require a dummy Show instance for E (a -> b) and IncoherentInstances
  -- Fun :: String -> E (a -> b)
  -- App :: (Show a, Show b) => E (a -> b) -> E a -> E b
  -- XY, Mouse are just globals, not their own types
  -- RGB, A etc aliases for XYZ, W etc
  Share :: GlslType a => StableName (E a) -> E a -> E a
  ShareRef :: GlslType a => Int -> E a

type Dist = E Float

-- deriving instance Generic a => Generic (E a)

-- instance NFData E

instance Show (StableName a) where
  show sn = show (hashStableName sn)

sh :: GlslType a => E a -> E a
sh e = Share sn e
  where sn = unsafePerformIO $ makeStableName e

-- -- DSL for GLSL programs
-- data GLSL = GLSL [UniformDecl] [Func]
-- data UniformDecl = UniformDecl Ty String
-- -- Env is the arg declarations
-- data Func = Func String Env Ty [Stmt]
-- data VarDecl = VarDecl String Ty
-- type Env = [VarDecl]
-- data Stmt = Decl VarDecl E | Assign E E | Return E

sabs :: (Show a, GlslType a) => E a -> E a
sabs = Fun1 "abs"

instance Num (E Float) where
  (+) = Add
  (*) = Mul
  abs = sabs
  signum = error "signum not implemented"
  fromInteger i = KF (fromInteger i)
  negate = Neg

instance Num (E Double) where
  (+) = Add
  (*) = Mul
  abs = sabs
  signum = error "signum not implemented"
  fromInteger i = KD (fromInteger i)
  negate = Neg

(+^) :: (GlslType a, GlslType b, GlslType c, Promotable a b c) => E a -> E b -> E c
(+^) = Add

(-^) :: (GlslType a, GlslType b, GlslType c, Promotable a b c) => E a -> E b -> E c
(-^) = Sub

(*^) :: (GlslType a, GlslType b, GlslType c, Promotable a b c) => E a -> E b -> E c
(*^) = Mul

(/^) :: (GlslType a, GlslType b, GlslType c, Promotable a b c) => E a -> E b -> E c
(/^) = Div

-- -- fromRational, (recip | (/))
-- instance Fractional (E a) where
--   fromRational i = KF (fromRational i)
--   (/) = undefined

-- fromRational, (recip | (/))
instance Fractional (E Float) where
  fromRational i = KF (fromRational i)
  (/) = Div

instance Fractional (E Double) where
  fromRational i = KD (fromRational i)
  (/) = Div

infix 4 ==.
(==.) :: (Show a, GlslType a) => E a -> E a -> E Bul
(==.) = Comparison "=="
infix 4 <.
(<.) :: (Show a, GlslType a) => E a -> E a -> E Bul
(<.) = Comparison "<"
infix 4 >.
(>.) :: (Show a, GlslType a) => E a -> E a -> E Bul
(>.) = Comparison ">"
infix 4 <=.
(<=.) :: (Show a, GlslType a) => E a -> E a -> E Bul
(<=.) = Comparison "<="
infix 4 >=.
(>=.) :: (Show a, GlslType a) => E a -> E a -> E Bul
(>=.) = Comparison ">="

data Swizzler v where
  SW2 :: String -> Swizzler (V2 a)
  SW3 :: String -> Swizzler (V3 a)
swizzleFields :: Swizzler v -> String
swizzleFields (SW2 s) = s
swizzleFields (SW3 s) = s

-- data Fielder = Fielder String
data Fielder v where
  Fielder :: String -> Fielder a

_x :: (GlslType (v n), FieldsTo (v n) n) => E (v n) -> E n
_x e = Field (Fielder "x") e
_y e = Field (Fielder "y") e
xy :: (GlslType (V2 n), GlslType (vv n), SwizzlesTo (vv n) (V2 n)) => E (vv n) -> E (V2 n)
xy e = Swizzle (SW2 "xy") e
yx e = Swizzle (SW2 "yx") e
xyz e = Swizzle (SW3 "xyz") e
yxz e = Swizzle (SW3 "yxz") e

rgb e = Swizzle (SW3 "rgb") e
_a :: (GlslType (v n), FieldsTo (v n) n) => E (v n) -> E n
_a e = Field (Fielder "a") e

deriving instance Show v => Show (Swizzler v)
deriving instance Show v => Show (Fielder v)

-- SwizzlesTo x y means that an x can be swizzled to a y
class (Show a, Show b) => SwizzlesTo a b
instance SwizzlesTo (V4 a) (V4 a)
instance SwizzlesTo (V4 a) (V3 a)
instance SwizzlesTo (V4 a) (V2 a)
instance SwizzlesTo (V3 a) (V3 a)
instance SwizzlesTo (V3 a) (V2 a)
instance SwizzlesTo (V2 a) (V2 a)

class (Show a, Show b) => FieldsTo a b
instance Show a => FieldsTo (V4 a) a
instance Show a => FieldsTo (V3 a) a
instance Show a => FieldsTo (V2 a) a

deriving instance Show a => Show (E a)

-- a `op` b results in type c
class (Show a, Show b, Show c) => Promotable a b c | a b -> c
instance Promotable Int Int Int
instance Promotable Int Float Float
instance Promotable Float Float Float
-- I have no idea if this is how GLSL does it
instance Promotable Double Double Double
instance Promotable Float Double Double
-- Could there be an abbreviation, like this?
--   class SelfPromotable a => Promotable a a a
instance Promotable (V2 Float) (V2 Float) (V2 Float)
instance Promotable (V3 Float) (V3 Float) (V3 Float)
instance Promotable (V2 Float) Float (V2 Float)
instance Promotable (V3 Float) Float (V3 Float)
instance Promotable (V4 Float) Float (V4 Float)
instance Promotable (V2 Double) (V2 Double) (V2 Double)
instance Promotable (V2 Double) Double (V2 Double)
instance Promotable (Mat2 Float) (V2 Float) (V2 Float)
instance Promotable Float (V4 Float) (V4 Float)
instance Promotable (V4 Float) (V4 Float) (V4 Float)

class Show a => Lengthable a b | a -> b
-- works
-- instance Lengthable (V2 Float) Float
-- instance Lengthable (V3 Float) Float
instance Lengthable (V2 a) a
instance Lengthable (V3 a) a

-- class Swizzleable a
-- instance Swizzleable (V2 a)
-- instance Swizzleable (V3 a)

class GlslType a where
  typeName :: E a -> String

instance GlslType Int where
  typeName _ = "int"
instance GlslType Float where
  typeName _ = "float"
instance GlslType Double where
  typeName _ = "double"
instance GlslType (V2 Float) where
  typeName _ = "vec2"
instance GlslType (V2 Double) where
  typeName _ = "dvec2"
instance GlslType (V3 Float) where
  typeName _ = "vec3"
instance GlslType (V3 Double) where
  typeName _ = "dvec3"
instance GlslType (V4 Float) where
  typeName _ = "vec4"
instance GlslType (Mat2 float) where
  typeName _ = "mat2"
instance GlslType Bul where
  typeName _ = "bool"

-- Not sure why I thought I needed this
-- instance Floating E

-- instance (Show a) => Random (E a)
instance Random (E Float)
instance Random (E Double)

instance (Show a) => UniformRange (E a) where
  uniformRM (KF lo, KF hi) g = KF <$> uniformRM (lo, hi) g
  uniformRM (Neg (KF lo), KF hi) g = KF <$> uniformRM (-lo, hi) g
  uniformRM (KF lo, Neg (KF hi)) g = KF <$> uniformRM (lo, -hi) g
  uniformRM (Neg (KF lo), Neg (KF hi)) g = KF <$> uniformRM (-lo, -hi) g
  uniformRM pr _ = error $ "UniformRange not implemented for: " ++ show pr

-- instance (Show a) => SRS.Uniform (E a)
instance SRS.Uniform (E Float)
  where
    -- This worked but seems unnecessary
    -- uniformM g = KF <$> uniformRM (0.0, 1.0) g
    uniformM g = uniformRM (0.0, 1.0) g

instance SRS.Uniform (E Double)
  where
    -- This worked but seems unnecessary
    -- uniformM g = KF <$> uniformRM (0.0, 1.0) g
    uniformM g = uniformRM (0.0, 1.0) g

type Shape = Transform -> E Float
type UnOp = Shape -> Shape
type BinOp = Shape -> Shape -> Shape

-- (Transform xy t)
data Transform = Transform (E (V2 Float)) (E Float)
type Transformer = Transform -> Transform

-- time :: E
-- time = (U (UF "time"))

-- addy :: E -> E -> E
-- addy a b = a + b

-- -- instance Hashable (StableName a) where
-- --   hash = hashStableName

-- mksn :: a -> (a, StableName a)
-- mksn x = (x, sn)
--   where sn = unsafePerformIO $ makeStableName x

-- snTest :: IO ()
-- snTest = do
--   let n = KF 12.3
--       sum = addy n n
--   msp sum
--   sn0 <- makeStableName sum
--   sn1 <- makeStableName sum
--   let sn2 = mksn sum
--   let sn3 = mksn sum
--   msp ("sns", sn0, sn1, sn2, sn3)
--   msp $ sn0 == sn1
--   let m0 = empty
--       m1 = insert sn0 sum m0
--   msp m1
--   msp $ m1 !? sn1
--   msp "hi snTest"
