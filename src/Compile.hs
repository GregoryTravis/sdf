module Compile
( compileSingle
, compileFunction ) where

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
typeOf refs Mouse = TV2
typeOf refs (ShareRef n) = typeOf refs (refs M.! n)
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
typeOf refs (RGB e) = TV3
typeOf refs (A e) = TF
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

-- It's assume the top level of any call to compileE is a call to parens
compileSubE :: Show a => E a -> String
compileSubE (KF n) = parens $ show n
compileSubE (Add a b) = op "+" (compileSubE a) (compileSubE b)
compileSubE (Sub a b) = op "-" (compileSubE a) (compileSubE b)
compileSubE (Mul a b) = op "*" (compileSubE a) (compileSubE b)
compileSubE (Div a b) = op "/" (compileSubE a) (compileSubE b)
compileSubE (V2 a b) = fun "vec2" [compileSubE a, compileSubE b]
compileSubE (V3 a b c) = fun "vec3" [compileSubE a, compileSubE b, compileSubE c]
compileSubE (V4 a b c d) = fun "vec4" [compileSubE a, compileSubE b, compileSubE c, compileSubE d]
compileSubE (Length e) = fun "length" [compileSubE e]
compileSubE (Uniform name) = parens name
compileSubE (Swizzle swizzler v) = dot (compileSubE v) (swizzleFields swizzler)
compileSubE (Field (Fielder field) v) = dot (compileSubE v) field
compileSubE (Fun1 name arg1) = fun name [compileSubE arg1]
compileSubE (Fun2 name arg1 arg2) = fun name [compileSubE arg1, compileSubE arg2]
compileSubE (Fun3 name arg1 arg2 arg3) = fun name [compileSubE arg1, compileSubE arg2, compileSubE arg3]
compileSubE (Neg a) = parens $ concat ["-", compileSubE a]
compileSubE (Mat2 xs) = fun "mat2" (map compileSubE xs)
compileSubE (Comparison name a b) = op name (compileSubE a) (compileSubE b)
compileSubE (Cond b t e) = cond (compileSubE b) (compileSubE t) (compileSubE e)
compileSubE e@(Share _ _) = error $ "Can't compile Sh nodes: " ++ show e
-- compileSubE (Share _ e) = compileSubE e
compileSubE (ShareRef n) = parens $ subexp n
compileSubE e = error $ "compileSubE: " ++ show e

-- Compile an expression to a list of bindings, with the specified name for the
-- outermost expression binding.
compileE :: (Show a, GlslType a) => String -> E a -> String
compileE topVar e = renderBindings (compileEToBindings topVar e)

compileEToBindings :: (Show a, GlslType a) => String -> E a -> [(String, String, String)]
compileEToBindings topVar e =
  let (e', (_, seMap)) = runState (share' e) initState
      compiledE = compileSubE e'
      ty = typeName e'
  in toBindings seMap ++ [(topVar, ty, compiledE)]

renderBindings :: [(String, String, String)] -> String
renderBindings bindings = intercalate ";\n" lines ++ ";\n"
  where lines = map renderBinding bindings

renderBinding :: (String, String, String) -> String
renderBinding (var, ty, se) = concat [ty, " ", var, " = ", se]

toBindings :: SEMap -> [(String, String, String)]
toBindings seMap = map f sortedElems
  where f (n, ty, se) = (subexp n, ty, se)
        sortedElems = sortOn fst3 (HM.elems seMap)
        fst3 (a, _, _) = a

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

compileSingle :: E -> String
compileSingle e = compileGroup (share e) "topColor"

-- Should add return, functions, etc to the DSL
compileFunction :: E -> String
compileFunction e =
  let top = "toppppp"
      bindings = compileGroup (share e) top
      -- all = "vec4 " ++ name ++ "() {\n" ++ bindings ++ "return " ++ top ++ ";\n}\n"
      all = bindings ++ "return " ++ top ++ ";\n"
   in all
