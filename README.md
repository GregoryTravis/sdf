SDF: GPU screensaver implemented in Haskell

======

<p align="center">
  <img src="https://raw.githubusercontent.com/GregoryTravis/sdf/master/images/screencast.gif">
</p>

SDF is a GPU screensaver implemented in Haskell.

It uses 2D [signed distance
functions](https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm)
to define primitive shapes and shape combinators, and renders them anti-aliased.

The simple Javascript client downloads new randomly-generated animated shapes
from the server every five seconds, seamlessly blending them in the browser,
resulting in an animation that never repeats.

It's built around a DSL that compiles pixel shaders to GLSL. It's core
expression type is not refined by a GLSL type, so some type errors are caught by
the DSL's crude typechecker, and the rest by the GLSL compiler.

The above animation is:

```
filaoa :: Shape
filaoa = scale 0.1 $ smoothUnion (scale (time / 10.0) filaoa') (rotation (time / 10.0) filaoa')
  where filaoa' = pfGrid 2.25 2.25 circle
```

Which is compiled into:

```
"vec4 x0 = (vec4((0.0), (0.0), (0.0), (1.0)));
float x1 = ((((((((((uv))/((1.0))))/((0.1))))/((((time))/((10.0))))).x))/((2.25)));
float x2 = (floor((x1)));
float x3 = (x2);
float x4 = (mod((abs((x3))), (2.0)));
float x5 = (((((x1))+((-(x2)))))*((2.25)));
float x6 = (((((x4))==((1.0))))?((((2.25))+((-(x5))))):((x5)));
float x7 = ((((((((((uv))/((1.0))))/((0.1))))/((((time))/((10.0))))).y))/((2.25)));
[... continues ...]
```

[Live demo here.](http://34.148.179.16:8000/infinity.html)

TODO:

* Add a GLSL type parameter to the main expression type to support (and typecheck) GLSL ad-hoc polymorphism
* Monad comprehensions for loops

Thanks to https://twgljs.org/.

<p align="center">
  <img src="https://raw.githubusercontent.com/GregoryTravis/sdf/main/images/ss0.png">
</p>

<p align="center">
  <img src="https://raw.githubusercontent.com/GregoryTravis/sdf/main/images/ss1.png">
</p>

<p align="center">
  <img src="https://raw.githubusercontent.com/GregoryTravis/sdf/main/images/ss2.png">
</p>

<p align="center">
  <img src="https://raw.githubusercontent.com/GregoryTravis/sdf/main/images/ss3.png">
</p>
