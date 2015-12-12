module LudumDare where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Int (toNumber)
import Data.Matrix (toArray)
import Data.Matrix4 (identity, makePerspective, translate)
import Data.Vector3 (vec3)
import Graphics.WebGLAll
import Prelude ((/), bind, negate, Unit())

shaders :: Shaders {
    aVertexPosition :: Attribute Vec3,
    uPMatrix :: Uniform Mat4,
    uMVMatrix :: Uniform Mat4
}
shaders = Shaders
    """
    precision mediump float;

    void main(void) {
        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
    """

    """
    attribute vec3 aVertexPosition;
    uniform mat4 uMVMatrix;
    uniform mat4 uPMatrix;

    void main(void) {
         gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
    }
    """

main :: Eff (console :: CONSOLE) Unit
main = runWebGL "glcanvas" (\s -> log s) \ context -> do
    withShaders shaders (\s -> log s) \ bindings -> do
        clearColor 0.17 0.47 0.24 1.0
        enable DEPTH_TEST

        canvasWidth <- getCanvasWidth context
        canvasHeight <- getCanvasHeight context
        viewport 0 0 canvasWidth canvasHeight
        clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

        let pMatrix = makePerspective 45.0
                      (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
        setUniformFloats bindings.uPMatrix (toArray pMatrix)

        let mvMatrix = translate (vec3 (-1.5) 0.0 (-7.0)) identity
        setUniformFloats bindings.uMVMatrix (toArray mvMatrix)

        buf1 <- makeBufferFloat [0.0, 1.0, 0.0,
                                (-1.0), (-1.0), 0.0,
                                1.0, (-1.0), 0.0]
        drawArr TRIANGLES buf1 bindings.aVertexPosition

        log "Done"
