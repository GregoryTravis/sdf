{-# LANGUAGE AllowAmbiguousTypes, DeriveGeneric, EmptyDataDeriving, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, StandaloneDeriving #-}

module Typed
( typedMain ) where

import Data.List (intercalate)

import Util hiding (time)

-- Use DataKinds?
data V2 a
  deriving Show
data V3 a
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
  SW2 :: (Show (v b), Show b) => Swizzler2 -> E (v b) -> E (V2 b)
  SW3 :: (Show (v b), Show b) => Swizzler3 -> E (v b) -> E (V3 b)
  Uniform :: String  -> E a
  -- XY, Mouse: these are just a special V2
  -- Swizzle :: (Show a, Show b, SwizzlesTo a (v b)) => Swizzler b -> E a -> E (v b)
  -- Swizzle :: (Show a, Show b, SwizzlesTo a b) => Swizzler b -> E a -> E b
  -- Swizzle :: SwizzlesTo a b => String -> E a -> E b
  -- Swizzle :: Swizzleable a => String -> E a -> E a

-- TODO: Maybe don't need these since SWn encodes the output type, although
-- maybe we need this so we can't forge a bad one
data Swizzler2 = Swizzler2 String
  deriving Show

data Swizzler3 = Swizzler3 String
  deriving Show

xy v = SW2 (Swizzler2 "xy") v
yx v = SW2 (Swizzler2 "yx") v
xyz v = SW3 (Swizzler3 "xyz") v
yxz v = SW3 (Swizzler3 "yxz") v

-- data Swizzler a where
--   Swizzler2 :: String -> Swizzler (V2 a)

-- deriving instance Show a => Show (Swizzler a)

-- class (Show a, Show b) => SwizzlesTo a b
-- instance SwizzlesTo (V3 a) (V3 a)
-- instance SwizzlesTo (V3 a) (V2 a)
-- instance Show a => SwizzlesTo (V3 a) a
-- instance SwizzlesTo (V2 a) (V2 a)
-- instance Show a => SwizzlesTo (V2 a) a

-- yx :: E (v a) -> E (V2 a)
-- yx = Swizzle $ Swizzler2 "yx"

-- -- TODO: Should make it impossible to fake these by using a Swizzler NT instead of a string?
-- yx :: SwizzlesTo a (V2 b) => Typed.E a -> Typed.E (V2 b)
-- yx e = Swizzle "yx" e
-- x e = Swizzle "x" e

-- yx :: E (V2 a) -> E (V2 a)
-- yx e = Swizzle "yx" e

deriving instance Show a => Show (E a)

-- TODO: maybe "Promotable"?
class (Show a, Show b, Show c) => Addable a b c | a b -> c
instance Addable Int Int Int
instance Addable Int Float Float
instance Addable Float Float Float
-- I have no idea if this is how GLSL does it
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

-- blah :: E Int
blahii = Add (KI 1) (KI 2)
blahif = Add (KI 1) (KF 2.0)
blahvv = Add (V2 (KF 1.0) (KF 1.0)) (V2 (KF 1.0) (KF 1.0))
blahvf = Add (V2 (KF 1.0) (KF 1.0)) (KF 2.0)
blahdd = Add (V2 (KD 1.0) (KD 1.0)) (V2 (KD 1.0) (KD 1.0))
blahdf = Add (V2 (KD 1.0) (KD 1.0)) (KD 2.0)
blahfd = Add (KF 1.0) (KD 2.0)

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

parens :: String -> String
parens x = concat ["(", x, ")"]

op :: String -> String -> String -> String
op operator a b = parens $ concat [parens a, operator, parens b]

fun :: String -> [String] -> String
fun f args = parens $ concat [f, parens arglist]
  where arglist = intercalate ", " args

dot :: String -> String -> String
dot e field = parens $ concat [e, ".", field]

compileE :: E a -> String
compileE (KF n) = parens $ show n
compileE (Add a b) = op "+" (compileE a) (compileE b)
compileE (V2 a b) = fun "vec2" [compileE a, compileE b]
compileE (V3 a b c) = fun "vec3" [compileE a, compileE b, compileE c]
compileE (Length e) = fun "length" [compileE e]
compileE (SW2 (Swizzler2 fields) v) = dot (compileE v) fields
compileE (SW3 (Swizzler3 fields) v) = dot (compileE v) fields
compileE (Uniform name) = parens name

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
  etc f
  etc v2
  etc v3
  etc $ V2 (Add (KF 1.0) (KF 2.0)) (KF 3.0)
  etc $ Length v2
  etc $ Length v3
  etc $ xy v2
  etc $ xy v3
  etc $ yx v2
  etc $ yx v3
  etc $ xyz v3
  etc $ yxz v3
  etc $ time
  -- etc $ x v2

  -- works
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
