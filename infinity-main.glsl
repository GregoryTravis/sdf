precision mediump float;

uniform vec2 resolution;
uniform float globalTime;
uniform int step;
uniform float playDuration;
uniform float transitionDuration;

// TODO slow
float linmap(float a, float b, float c, float d, float x) {
  return ((x - a) * ((d - c) / (b - a))) + c;
}

FUNCTIONS_HERE

void main() {
  // Assumes horizontal
  // float playDuration = 2.0;
  // float transitionDuration = 1.0;
  float m = (resolution.x - resolution.y) / 2.0;
  vec2 uv = vec2(linmap(resolution.x/2.0, resolution.x - m, 0.0, 1.0, gl_FragCoord.x),
                 linmap(resolution.y/2.0, resolution.y    , 0.0, 1.0, gl_FragCoord.y));

  float oneT = globalTime - (float(step) * (playDuration + transitionDuration));
  float twoT = globalTime - ((float(step) + 1.0) * (playDuration + transitionDuration));
  vec4 onec = one(uv, oneT);
  vec4 twoc = two(uv, twoT);
  //vec4 topColor = onec * 0.5 + twoc * 0.5;
  float transitionStart = (playDuration + transitionDuration) * float(step) + playDuration;
  float transitionEnd = transitionStart + transitionDuration;
  float alpha = smoothstep(transitionStart, transitionEnd, globalTime);
  vec4 topColor = (1.0 - alpha) * onec + alpha * twoc;

      // bwBlend = smoothstep (-smoothRadius) smoothRadius dist

  gl_FragColor = topColor;
}