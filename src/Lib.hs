{-# LANGUAGE FlexibleContexts #-}

module Lib where

import E

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
smod = Fun2 "mod"
smin :: E Float -> E Float -> E Float
smin = Fun2 "min"
smax :: E Float -> E Float -> E Float
smax = Fun2 "max"

sdFdx :: E Float -> E Float
sdFdx = Fun1 "dFdx"
sdFdy :: E Float -> E Float
sdFdy = Fun1 "dFdy"

norm2 :: E (V2 Float) -> E (V2 Float)
norm2 v = v /^ (Length v)

norm3 :: E (V3 Float) -> E (V3 Float)
norm3 v = v /^ (Length v)

dot2 :: E (V2 Float) -> E (V2 Float) -> E Float
dot2 = Fun2 "dot"

dot3 :: E (V3 Float) -> E (V3 Float) -> E Float
dot3 = Fun2 "dot"

-- vec3 :: E (V2 Float) -> E (V3 Float)
-- vec3 = Fun1 "vec3"

-- smoothstep edge0 edge1 x = sh $ Fun "smoothstep" [TF, TF, TF] TF [edge0, edge1, x]
smoothstep :: E Float -> E Float -> E Float -> E Float
smoothstep = Fun3 "smoothstep"

-- types are for alpha blending
-- mix x y a = sh $ Fun "mix" [TV3, TV3, TF] TV3 [x, y, a]
mix :: (Show a, GlslType a, GlslType (V3 a)) => E (V3 a) -> E (V3 a) -> E a -> E (V3 a)
mix = Fun3 "mix"
-- specific to (rgb, a)
-- vec4 rgb a = sh $ Fun "vec4" [TV3, TF] TV4 [rgb, a]
vec4 :: (Show a, GlslType a, GlslType (V3 a), GlslType (V4 a)) => E (V3 a) -> E a -> E (V4 a)
vec4 = Fun2 "vec4"

time :: E Float
time = Uniform "time"
uv :: E (V2 Float)
uv = Uniform "uv"
mouse :: E (V2 Float)
mouse = Uniform "mouse"
