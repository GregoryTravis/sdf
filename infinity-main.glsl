precision mediump float;

uniform vec2 resolution;
uniform float time;

// TODO slow
float linmap(float a, float b, float c, float d, float x) {
  return ((x - a) * ((d - c) / (b - a))) + c;
}

FUNCTIONS_HERE

void main() {
  // Assumes horizontal
  float m = (resolution.x - resolution.y) / 2.0;
  vec2 uv = vec2(linmap(resolution.x/2.0, resolution.x - m, 0.0, 1.0, gl_FragCoord.x),
                 linmap(resolution.y/2.0, resolution.y    , 0.0, 1.0, gl_FragCoord.y));

  vec4 onec = one(uv);
  vec4 twoc = two(uv);
  //vec4 topColor = onec * 0.5 + twoc * 0.5;
  float alpha = smoothstep(15.0, 20.0, time);
  vec4 topColor = (1.0 - alpha) * onec + alpha * twoc;

      // bwBlend = smoothstep (-smoothRadius) smoothRadius dist

  gl_FragColor = topColor;
}