{-# LANGUAGE AllowAmbiguousTypes, DeriveGeneric, EmptyDataDeriving, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, StandaloneDeriving #-}

module Typed
( E(..)
, V4(..)
, typedMain
, GlslType
, typeName
, (+^)
, (-^)
, (*^)
, sh
, evalShape
, Fielder(..)
, swizzleFields
, filaoa ) where

import Control.Monad (when)
import GHC.Generics (Generic)
import System.IO.Unsafe
import System.Mem.StableName

import Util hiding (time)

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

-- deriving instance Generic a => Generic (E a)

sh :: GlslType a => E a -> E a
sh e = Share sn e
  where sn = unsafePerformIO $ makeStableName e

instance Num (E Float) where
  (+) = Add
  (*) = Mul
  abs = error "abs"
  signum = error "signum not implemented"
  fromInteger i = KF (fromInteger i)
  negate = Neg

instance Num (E Double) where
  (+) = Add
  (*) = Mul
  abs = error "abs"
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

instance Show (StableName a) where
  show sn = show (hashStableName sn)

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

ssqrt :: E Float -> E Float
ssqrt x = Fun1 "sqrt" x
satan :: E Float -> E Float -> E Float
satan y x = Fun2 "atan" y x
ssin :: E Float -> E Float
ssin = Fun1 "sin"
scos :: E Float -> E Float
scos = Fun1 "cos"

sfloor :: E Float -> E Float
sfloor = Fun1 "floor"
smod :: E Float -> E Float -> E Float
smod = (Fun2 "mod")
sabs :: E Float -> E Float
sabs = Fun1 "abs"
smin :: E Float -> E Float -> E Float
smin = Fun2 "min"
smax :: E Float -> E Float -> E Float
smax = Fun2 "max"

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
xy :: (GlslType (V2 n), GlslType (vv n), SwizzlesTo (vv n) (V2 n)) => Typed.E (vv n) -> Typed.E (V2 n)
xy e = Swizzle (SW2 "xy") e
yx e = Swizzle (SW2 "yx") e
xyz e = Swizzle (SW3 "xyz") e
yxz e = Swizzle (SW3 "yxz") e

deriving instance Show v => Show (Swizzler v)
deriving instance Show v => Show (Fielder v)

-- SwizzlesTo x y means that an x can be swizzled to a y
class (Show a, Show b) => SwizzlesTo a b
instance SwizzlesTo (V3 a) (V3 a)
instance SwizzlesTo (V3 a) (V2 a)
instance SwizzlesTo (V2 a) (V2 a)

class (Show a, Show b) => FieldsTo a b
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

-- Show expression and type
eat :: (GlslType a, Show a) => E a -> IO ()
eat e = do
  msp e
  msp (typeName e)

-- Show expression, type, and compiled expressions
-- etc :: (GlslType a, Show a) => E a -> IO ()
-- etc e = do
--   msp e
--   msp (typeName e)
--   msp $ compileE e

time :: E Float
time = Uniform "time"
uv :: E (V2 Float)
uv = Uniform "uv"

tstVerbose = False

tst :: (Eq a, Show a) => String -> a -> a -> IO ()
tst info e a = do
  when tstVerbose $
    msp ("tsteq", info, e, a)
  when (a /= e) $
    msp $ "Test Failure: " ++ info ++ ": " ++ (show e) ++ " /= " ++ (show a)

tstEq :: (Eq a, Show a) => a -> a -> IO ()
tstEq = tst "equal"
-- tstEq e a = do
--   msp ("tsteq", e, a)
--   when (a /= e) $
--     msp $ "Test Failure: " ++ (show e) ++ "/=" ++ (show a)

tstType :: (GlslType a, Eq a, Show a) => E a -> String -> IO ()
tstType e a = tst info (typeName e) a
  where info = "typeName " ++ show e

-- FAVORITE don't lose this!!
-- fall in love all over again
filaoa :: Shape
filaoa = scale (KF 0.1) $ smoothUnion (scale (time /^ (KF 10.0)) filaoa') (rotation (time /^ (KF 10.0)) filaoa')
  where filaoa' = pfGrid (KF 2.25) (KF 2.25) circle

transform :: Transformer -> Shape -> Shape
transform transformer p = p . transformer

idTransform :: Transform
idTransform = Transform uv time

evalShape :: Shape -> E Float
evalShape p = p idTransform

scale :: E Float -> UnOp
scale s = transform (scale' s)

scale' :: E Float -> Transformer
scale' s (Transform xy t) = Transform (xy /^ s) t

translation :: E (V2 Float) -> UnOp
translation dxy = transform (translation' dxy)

translation' :: E (V2 Float) -> Transformer
translation' dxy (Transform xy t) = Transform (xy -^ dxy) t

rotation :: E Float -> UnOp
rotation ang = transform (rotation' ang)

rotation' :: E Float -> Transformer
rotation' ang (Transform xy t) =
  let c = sh $ scos ang
      s = sh $ ssin ang
      mat = sh $ Mat2 [c, s, -s, c]
   in Transform (mat *^ xy) t

pfGrid :: E Float -> E Float -> UnOp
pfGrid w h = transform (pfGrid' w h)

-- I tried using mod to calculate xx, yy, xi, yi, but it was wrong, so I just inlined the original rust grid_fmod2()
pfGrid' :: E Float -> E Float -> Transformer
pfGrid' w h (Transform xy t) =
  let x = _x xy
      y = _y xy
      xow = sh $ x /^ w
      yoh = sh $ y /^ h
      xx = sh $ (xow -^ sfloor xow) * w
      xi = sh $ sfloor xow
      yy = sh $ (yoh -^ sfloor yoh) * h
      yi = sh $ sfloor yoh
      xx2 = sh $ Cond (smod (sabs xi) (KF 2.0) ==. (KF 1.0)) (w -^ xx) xx
      yy2 = sh $ Cond (smod (sabs yi) (KF 2.0) ==. (KF 1.0)) (h -^ yy) yy
   in Transform (V2 xx2 yy2) t

circle :: Shape
circle (Transform xy _) =
  let dist = Length xy -^ KF 1.0
   in dist

type Shape = Transform -> E Float
type UnOp = Shape -> Shape
type BinOp = Shape -> Shape -> Shape

data Transform = Transform (E (V2 Float)) (E Float)
type Transformer = Transform -> Transform

binopper :: (E Float -> E Float -> E Float) -> BinOp
binopper distCombiner p0 p1 tr = distCombiner (sh $ p0 tr) (sh $ p1 tr)

smoothUnion :: BinOp
smoothUnion = binopper smoothUnion'

smoothUnion' :: E Float -> E Float -> E Float
smoothUnion' usd0 usd1 =
  let d0 = sh usd0
      d1 = sh usd1
      r = KF 0.3
      md0 = sh $ smin (d0 -^ r) (KF 0.0)
      md1 = sh $ smin (d1 -^ r) (KF 0.0)
      inside_distance = sh $ - (ssqrt $ (md0 *^ md0) +^ (md1 *^ md1))
      simple_union = sh $ smin d0 d1
      outside_distance = sh $ smax simple_union r
      dist = sh $ inside_distance + outside_distance
   in dist

tests = do
  tstType (Add (KI 1) (KI 2)) "int"
  tstType (Add (KI 1) (KF 2.0)) "float"
  tstType (Add (V2 (KF 1.0) (KF 1.0)) (V2 (KF 1.0) (KF 1.0))) "vec2"
  tstType (Add (V2 (KF 1.0) (KF 1.0)) (KF 2.0)) "vec2"
  tstType (Add (V2 (KD 1.0) (KD 1.0)) (V2 (KD 1.0) (KD 1.0))) "dvec2"
  tstType (Add (V2 (KD 1.0) (KD 1.0)) (KD 2.0)) "dvec2"

  tstType ((KI 1) +^ (KI 2)) "int"
  tstType ((KI 1) +^ (KF 2.0)) "float"
  tstType ((V2 (KF 1.0) (KF 1.0)) +^ (V2 (KF 1.0) (KF 1.0))) "vec2"
  tstType ((V2 (KF 1.0) (KF 1.0)) +^ (KF 2.0)) "vec2"
  tstType ((V2 (KD 1.0) (KD 1.0)) +^ (V2 (KD 1.0) (KD 1.0))) "dvec2"
  tstType ((V2 (KD 1.0) (KD 1.0)) +^ (KD 2.0)) "dvec2"

  let f = KF 1.0
      f2 = KF 2.0
  let v2 = V2 (KF 1.0) (KF 1.0)
      v2' = V2 (KF 2.0) (KF 2.0)
      v3 = V3 (KF 2.0) (KF 2.0) (KF 3.0)
      m2 = Mat2 [(KF 2.0), (KF 2.0), (KF 2.0), (KF 2.0)]
  tstType f "float"
  tstType f2 "float"
  tstType v2 "vec2"
  tstType v2' "vec2"
  tstType v3 "vec3"
  tstType m2 "mat2"
  tstType (f +^ f) "float"
  tstType (f +^ KF 1.0) "float"
  tstType (v2 -^ KF 1.0) "vec2"
  tstType (Length (xy v3) -^ KF 1.0) "float"

  tstType (ssqrt f) "float"
  tstType (ssin f) "float"
  tstType (scos f) "float"
  tstType (satan (ssin f) (scos f)) "float"
  tstType (Neg f) "float"
  tstType (f ==. f2) "bool"
  tstType (f <. f2) "bool"
  tstType (f >. f2) "bool"
  tstType (f <=. f2) "bool"
  tstType (f >=. f2) "bool"
  tstType (Cond (f >=. f2) (ssin f) (scos f)) "float"

  tstType (V2 (Add (KF 1.0) (KF 2.0)) (KF 3.0)) "vec2"
  tstType (Length v2) "float"
  tstType (Length v3) "float"
  tstType (time) "float"
  tstType (xy v2) "vec2"
  tstType (xy v3) "vec2"
  tstType (yx v2) "vec2"
  tstType (yx v3) "vec2"
  tstType (xyz v3) "vec3"
  tstType (yxz v3) "vec3"
  tstType (_x v2) "float"
  tstType (_y v2) "float"
  tstType (_x v3) "float"
  tstType (_y v3) "float"
  tstType (_x v2) "float"

-- Experimenting with a type paramter for E.
typedMain = do
  tests

  msp "typed hi"
