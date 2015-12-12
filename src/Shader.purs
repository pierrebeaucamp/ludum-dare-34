module Shader where

import Graphics.WebGL

shaders :: Shaders {
    aMiter      :: Attribute Float,
    aNormal     :: Attribute Vec2,
    aPosition   :: Attribute Vec2,
    uModelView  :: Uniform Mat4,
    uProjection :: Uniform Mat4,
    uThickness  :: Uniform Float,
    uColor      :: Uniform Vec3,
    uInner      :: Uniform Float
}
shaders = Shaders
    """
    precision mediump float;

    uniform vec3 uColor;
    uniform float uInner;
    varying float vEdge;

    void main() {
        float v = 1.0 - abs(vEdge);
        v = smoothstep(0.65, 0.7, v * uInner);
        gl_FragColor = mix(vec4(uColor, 1.0), vec4(0.0), v);
    }
    """

    """
    attribute float aMiter;
    attribute vec2 aNormal;
    attribute vec2 aPosition;
    uniform mat4 uModelView;
    uniform mat4 uProjection;
    uniform float uThickness;
    varying float vEdge;

    void main() {
        vEdge = sign(aMiter);
        vec2 pntPos = aPosition.xy + vec2(aNormal * uThickness / 2.0 * aMiter);
        gl_Position = uProjection * uModelView * vec4(pntPos, 0.0, 1.0);
        gl_PointSize = 1.0;
    }
    """
