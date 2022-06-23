{-# LANGUAGE AllowAmbiguousTypes, DeriveGeneric, EmptyDataDeriving, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, StandaloneDeriving #-}

module Typed
( typedMain ) where

import Data.List (intercalate)
import System.Mem.StableName

import Util hiding (time)

-- Use DataKinds?
data V2 a
  deriving Show
data V3 a
  deriving Show
data Mat2 a
  deriving Show
data Bul
  deriving Show

-- goals
-- data E = KF Double | U Uniform | Add E E | Sub E E | Mul E E | Div E E | Length E | V2 E E | V3 E E E | V4 E E E E | XY | Mouse
--        | Abs E | Min E E | Max E E | X E | Y E | Neg E | Fun1 String Ty Ty E | Fun2 String Ty Ty Ty E E | Fun String [Ty] Ty [E] | Mat2 [E]
--        | Comparison String E E | Cond E E E -- | V String
--        | RGB E | A E
--        | Share E (StableName E)
--        | ShareRef Int

data E a where
  KF :: Float -> E Float
  KD :: Double -> E Double
  KI :: Int -> E Int
  V2 :: Show a => E a -> E a -> E (V2 a)
  V3 :: Show a => E a -> E a -> E a -> E (V3 a)
  Add :: Addable a b c => E a -> E b -> E c
  Length :: Lengthable a b => E a -> E b
  Uniform :: String  -> E a
  Swizzle :: SwizzlesTo (vv n) (v n) => Swizzler (v n) -> E (vv n) -> E (v n)
  Field :: FieldsTo (vv n) n => Fielder n -> E (vv n) -> E n
  Fun1 :: Show a => String -> E a -> E b
  Fun2 :: (Show a, Show b) => String -> E a -> E b -> E c
  Neg :: E a -> E a
  Mat2 :: Show a => [E a] -> E (Mat2 a)
  Comparison :: Show a => String -> E a -> E a -> E Bul
  Cond :: E Bul -> E a -> E a -> E a
  -- These require a dummy Show instance for E (a -> b) and IncoherentInstances
  -- Fun :: String -> E (a -> b)
  -- App :: (Show a, Show b) => E (a -> b) -> E a -> E b
  -- XY, Mouse are just globals, not their own types
  -- RGB, A etc aliases for XYZ, W etc
  Share :: E a -> StableName (E a) -> E a
  ShareRef :: Int -> E a

instance Num (E Float) where
  (+) = Add
  (*) = undefined
  abs = undefined
  signum = error "signum not implemented"
  fromInteger i = KF (fromInteger i)
  negate = Neg

instance Num (E Double) where
  (+) = Add
  (*) = undefined
  abs = undefined
  signum = error "signum not implemented"
  fromInteger i = KD (fromInteger i)
  negate = Neg

-- -- fromRational, (recip | (/))
-- instance Fractional (E a) where
--   fromRational i = KF (fromRational i)
--   (/) = undefined

instance Show (StableName a) where
  show sn = show (hashStableName sn)

infix 4 ==.
(==.) :: Show a => E a -> E a -> E Bul
(==.) = Comparison "=="
infix 4 <.
(<.) :: Show a => E a -> E a -> E Bul
(<.) = Comparison "<"
infix 4 >.
(>.) :: Show a => E a -> E a -> E Bul
(>.) = Comparison ">"
infix 4 <=.
(<=.) :: Show a => E a -> E a -> E Bul
(<=.) = Comparison "<="
infix 4 >=.
(>=.) :: Show a => E a -> E a -> E Bul
(>=.) = Comparison ">="

ssqrt :: E Float -> E Float
ssqrt x = Fun1 "sqrt" x
satan :: E Float -> E Float -> E Float
satan y x = Fun2 "atan" y x
ssin :: E Float -> E Float
ssin = Fun1 "sin"
scos :: E Float -> E Float
scos = Fun1 "cos"

data Swizzler v where
  SW2 :: String -> Swizzler (V2 a)
  SW3 :: String -> Swizzler (V3 a)
swizzleFields :: Swizzler v -> String
swizzleFields (SW2 s) = s
swizzleFields (SW3 s) = s

-- data Fielder = Fielder String
data Fielder v where
  Fielder :: String -> Fielder a

x e = Field (Fielder "x") e
y e = Field (Fielder "y") e
xy :: SwizzlesTo (vv n) (V2 n) => Typed.E (vv n) -> Typed.E (V2 n)
xy e = Swizzle (SW2 "xy") e
yx e = Swizzle (SW2 "yx") e
xyz e = Swizzle (SW2 "xyz") e
yxz e = Swizzle (SW2 "yxz") e

deriving instance Show v => Show (Swizzler v)
deriving instance Show v => Show (Fielder v)

class (Show a, Show b) => SwizzlesTo a b
instance SwizzlesTo (V3 a) (V2 a)
instance SwizzlesTo (V2 a) (V2 a)

class (Show a, Show b) => FieldsTo a b
instance Show a => FieldsTo (V3 a) a
instance Show a => FieldsTo (V2 a) a

deriving instance Show a => Show (E a)

-- TODO: maybe "Promotable"?
class (Show a, Show b, Show c) => Addable a b c | a b -> c
instance Addable Int Int Int
instance Addable Int Float Float
instance Addable Float Float Float
-- I have no idea if this is how GLSL does it
instance Addable Double Double Double
instance Addable Float Double Double
-- Could there be an abbreviation, like this?
--   class SelfAddable a => Addable a a a
instance Addable (V2 Float) (V2 Float) (V2 Float)
instance Addable (V3 Float) (V3 Float) (V3 Float)
instance Addable (V2 Float) Float (V2 Float)
instance Addable (V2 Double) (V2 Double) (V2 Double)
instance Addable (V2 Double) Double (V2 Double)

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
etc :: (GlslType a, Show a) => E a -> IO ()
etc e = do
  msp e
  msp (typeName e)
  msp $ compileE e

subexp :: Int -> String
subexp n = "x" ++ show n

parens :: String -> String
parens x = concat ["(", x, ")"]

op :: String -> String -> String -> String
op operator a b = parens $ concat [parens a, operator, parens b]

fun :: String -> [String] -> String
fun f args = parens $ concat [f, parens arglist]
  where arglist = intercalate ", " args

dot :: String -> String -> String
dot e field = parens $ concat [e, ".", field]

cond :: String -> String -> String -> String
cond b t e = parens $ concat [parens b, "?", parens t , ":", parens e]

-- It's assume the top level of any call to compileE is a call to parens
compileE :: Show a => E a -> String
compileE (KF n) = parens $ show n
compileE (Add a b) = op "+" (compileE a) (compileE b)
compileE (V2 a b) = fun "vec2" [compileE a, compileE b]
compileE (V3 a b c) = fun "vec3" [compileE a, compileE b, compileE c]
compileE (Length e) = fun "length" [compileE e]
compileE (Uniform name) = parens name
compileE (Swizzle swizzler v) = dot (compileE v) (swizzleFields swizzler)
compileE (Field (Fielder field) v) = dot (compileE v) field
compileE (Fun1 name arg1) = fun name [compileE arg1]
compileE (Fun2 name arg1 arg2) = fun name [compileE arg1, compileE arg2]
compileE (Neg a) = parens $ concat ["-", compileE a]
compileE (Mat2 xs) = fun "mat2" (map compileE xs)
compileE (Comparison name a b) = op name (compileE a) (compileE b)
compileE (Cond b t e) = cond (compileE b) (compileE t) (compileE e)
compileE e@(Share _ _) = error $ "Can't compile Sh nodes: " ++ show e
compileE (ShareRef n) = parens $ subexp n

-- v2 = V2 (KF 1.0) (KF 1.0)

time :: E Float
time = Uniform "time"

-- Experimenting with a type paramter for E.
typedMain = do
  let f = KF 1.0
      f2 = KF 2.0
  let v2 = V2 (KF 1.0) (KF 1.0)
      v2' = V2 (KF 2.0) (KF 2.0)
      v3 = V3 (KF 2.0) (KF 2.0) (KF 3.0)
      m2 = Mat2 [(KF 2.0), (KF 2.0), (KF 2.0), (KF 2.0)]
  etc $ f + f
  -- etc $ f + 1.0
  -- etc $ v2 - 1.0
  -- etc $ Length xy - 1.0

  -- works
  -- etc $ ssqrt f
  -- etc $ ssin f
  -- etc $ scos f
  -- etc $ satan (ssin f) (scos f)
  -- etc $ Neg f
  -- etc $ m2
  -- etc $ f ==. f2
  -- etc $ f <. f2
  -- etc $ f >. f2
  -- etc $ f <=. f2
  -- etc $ f >=. f2
  -- etc $ Cond (f >=. f2) (ssin f) (scos f)

  -- works
  -- etc f
  -- etc v2
  -- etc v3
  -- etc $ V2 (Add (KF 1.0) (KF 2.0)) (KF 3.0)
  -- etc $ Length v2
  -- etc $ Length v3
  -- etc $ time
  -- etc $ xy v2
  -- etc $ xy v3
  -- etc $ yx v2
  -- etc $ yx v3
  -- etc $ xyz v3
  -- etc $ yxz v3
  -- etc $ x v2
  -- etc $ y v2
  -- etc $ x v3
  -- etc $ y v3
  -- etc $ x v2

  -- works
  -- let blahii = Add (KI 1) (KI 2)
  --     blahif = Add (KI 1) (KF 2.0)
  --     blahvv = Add (V2 (KF 1.0) (KF 1.0)) (V2 (KF 1.0) (KF 1.0))
  --     blahvf = Add (V2 (KF 1.0) (KF 1.0)) (KF 2.0)
  --     blahdd = Add (V2 (KD 1.0) (KD 1.0)) (V2 (KD 1.0) (KD 1.0))
  --     blahdf = Add (V2 (KD 1.0) (KD 1.0)) (KD 2.0)
  --     blahfd = Add (KF 1.0) (KD 2.0)
  -- eat blahii
  -- eat blahif
  -- eat blahvv
  -- eat blahvf
  -- eat blahdd
  -- eat blahdf
  -- eat blahfd
  -- let v2 = V2 (KF 1.0) (KF 1.0)
  --     v2' = V2 (KF 2.0) (KF 2.0)
  --     v3 = V3 (KF 2.0) (KF 2.0) (KF 3.0)
  -- eat $ Length v2
  -- eat $ v3
  -- eat $ Add v3 v3
  -- eat $ Length v3
  -- eat $ yx v2

  msp "typed hi"
