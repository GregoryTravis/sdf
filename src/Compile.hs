module Compile
( compile ) where

import Data.List (intercalate)
import qualified Data.Map as M

import E
import Share
import Transform

typeOf :: Refs -> E -> Ty
typeOf refs (KF _) = TF
typeOf refs (U (UF _)) = TF
typeOf refs (Add a b) = opType refs a b
typeOf refs (Sub a b) = opType refs a b
typeOf refs (Mul a b) = opType refs a b
typeOf refs (Div a b) = opType refs a b
typeOf refs (Length e) = mustType refs e [TV2] TF
typeOf refs (V2 _ _) = TV2
typeOf refs (V3 _ _ _) = TV3
typeOf refs (V4 _ _ _ _) = TV4
typeOf refs XY = TV2
typeOf refs (ShRef n) = typeOf refs (refs M.! n)
typeOf refs (Abs e) = typeOf refs e
typeOf refs (Min a b) = sameType refs a b
typeOf refs (Max a b) = sameType refs a b
typeOf refs (X e) = TF
typeOf refs (Y e) = TF
typeOf refs (Neg e) = mustType refs e [TF, TV2] TF
typeOf refs (Fun1 name tin tout arg) = mustType refs arg [tin] tout
typeOf refs (Fun2 name tin tin2 tout arg0 arg1) = tout -- should check both
typeOf refs (Fun name tins tout args) = tout -- should check both
typeOf refs (Mat2 _) = TM2
typeOf refs (Comparison _ _ _) = TB
typeOf refs (Cond _ t e) = sameType refs t e
-- typeOf refs (Var name) = sameType refs t e

glslType :: Ty -> String
glslType TF = "float"
glslType TV2 = "vec2"
glslType TV3 = "vec3"
glslType TV4 = "vec4"
glslType TM2 = "mat2"
glslType TB = "bool"

-- Figure out type of the overloaded arithmetic ops
opType :: Refs -> E -> E -> Ty
opType refs a b = go (typeOf refs a) (typeOf refs b)
  where go TF TF = TF
        go TV2 TF = TV2
        go TF TV2 = TV2
        go TV2 TV2 = TV2
        go TV3 TF = TV3
        go TF TV3 = TV3
        go TV3 TV3 = TV3
        go TV4 TF = TV4
        go TF TV4 = TV4
        go TV4 TV4 = TV4
        go TM2 TV2 = TV2
        go a b = error $ "opType? " ++ show a ++ " " ++ show b

-- The expression's type must be in the list; returns the last arg
mustType :: Refs -> E -> [Ty] -> Ty -> Ty
mustType refs e tys ty | (typeOf refs e) `elem` tys = ty
                  | otherwise = error $ "wrong type " ++ show e ++ " " ++ show tys

-- Types must be the same; returns the type
sameType :: Refs -> E -> E -> Ty
sameType refs a b | typeOf refs a == typeOf refs b = typeOf refs a
             | otherwise = error $ "type mismatch: " ++ show a ++ " " ++ show b

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

compileE :: E -> String
compileE (KF d) = parens $ show d
compileE (U (UF name)) = parens name
compileE (Add a b) = op "+" (compileE a) (compileE b)
compileE (Sub a b) = op "-" (compileE a) (compileE b)
compileE (Mul a b) = op "*" (compileE a) (compileE b)
compileE (Div a b) = op "/" (compileE a) (compileE b)
compileE (Length e) = fun "length" [compileE e]
compileE (V2 a b) = fun "vec2" [compileE a, compileE b]
compileE (V3 a b c) = fun "vec3" [compileE a, compileE b, compileE c]
compileE (V4 a b c d) = fun "vec4" [compileE a, compileE b, compileE c, compileE d]
compileE XY = "(uv)"
compileE e@(Sh _) = error $ "Can't compile Sh nodes: " ++ show e
compileE (ShRef n) = parens $ subexp n
compileE (Abs e) = fun "abs" [compileE e]
compileE (Min a b) = fun "min" [compileE a, compileE b]
compileE (Max a b) = fun "max" [compileE a, compileE b]
compileE (X e) = dot (compileE e) "x"
compileE (Y e) = dot (compileE e) "y"
compileE (Neg e) = parens $ concat ["-", compileE e]
compileE (Fun1 name _ _ arg) = fun name [compileE arg]
compileE (Fun2 name _ _ _ arg0 arg1) = fun name [compileE arg0, compileE arg1]
compileE (Fun name _ _ args) = fun name (map compileE args)
compileE (Mat2 es) = fun "mat2" (map compileE es)
compileE (Comparison opS a b) = op opS (compileE a) (compileE b)
compileE (Cond b t e) = cond (compileE b) (compileE t) (compileE e)

compileBinding :: Refs -> String -> E -> String
compileBinding refs var e = concat [ty, " ", var, " = ", compileE e]
  where ty = glslType $ typeOf refs e

compileBindings :: Refs -> [(String, E)] -> String
compileBindings refs bindings = intercalate ";\n" cbs ++ ";\n"
  where cbs = map (\(var, e) -> compileBinding refs var e) bindings

compileGroup :: (E, Refs) -> String -> String
compileGroup (top, refs) topName = compileBindings refs bindings
  where bindings =  shares ++ [(topName, top)]
        shares = map (\(n, e) -> (subexp n, e)) (M.toList refs)

compile :: E -> String
compile e = compileGroup (share e) "topColor"
