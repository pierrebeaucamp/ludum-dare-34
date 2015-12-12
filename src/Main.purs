module LudumDare where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.WebGL (EffWebGL())
import Data.ArrayBuffer.Types (Float32())
import Data.Date (now, Now(), toEpochMilliseconds)
import Data.Int (toNumber)
import Data.Matrix (toArray)
import Data.Matrix4 (identity, makePerspective, translate)
import Data.Maybe
import Data.Time
import Data.Vector3 (vec3)
import Graphics.WebGL
import Prelude

type State = {
    aPosition   :: Attribute Vec2,
    buf         :: Buffer Float32,
    context     :: WebGLContext,
    lastTime    :: Maybe Number,
    uModelView  :: Uniform Mat4,
    uProjection :: Uniform Mat4
}

gameLoop :: forall  eff. State -> EffWebGL (now :: Now | eff) Unit
gameLoop state = do
    render state
    state' <- update state
    return unit
    requestAnimationFrame (gameLoop state')

main :: Eff (console :: CONSOLE, now :: Now) Unit
main = runWebGL "glcanvas" (\s -> log s) \ context -> do
    withShaders shaders (\s -> log s) \ bindings -> do
        clearColor 0.17 0.47 0.24 1.0
        disable DEPTH_TEST
        disable CULL_FACE
        buf <- makeBufferFloat [0.0, 1.0, 0.0,
                                (-1.0), (-1.0), 0.0,
                                1.0, (-1.0), 0.0]

        gameLoop {
            aPosition: bindings.aPosition,
            buf: buf,
            context: context,
            lastTime: Nothing,
            uModelView: bindings.uModelView,
            uProjection: bindings.uProjection
        }

render :: forall eff. State -> EffWebGL (now :: Now | eff) Unit
render state = do
    canvasWidth <- getCanvasWidth state.context
    canvasHeight <- getCanvasHeight state.context
    viewport 0 0 canvasWidth canvasHeight
    clear [DEPTH_BUFFER_BIT]

    let pMatrix = makePerspective 45.0
                  (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
    setUniformFloats state.uProjection (toArray pMatrix)

    let mvMatrix = translate (vec3 (-1.5) 0.0 (-7.0)) identity
    setUniformFloats state.uModelView (toArray mvMatrix)

    drawArr TRIANGLES state.buf state.aPosition

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

unpackMilliseconds :: Milliseconds -> Number
unpackMilliseconds (Milliseconds n) = n

update :: forall eff. State -> EffWebGL (now :: Now | eff) State
update state = do
    timeNow <- liftM1 (unpackMilliseconds <<< toEpochMilliseconds) now
    return state {lastTime = Just timeNow}

