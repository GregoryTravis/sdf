module Test
( testMain ) where

import E
import Lib

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
testMain = do
  tests
  msp "typed hi"
