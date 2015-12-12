module LudumDare where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.WebGL (EffWebGL())
import Data.ArrayBuffer.Types (Float32())
import Data.Date (now, Now(), toEpochMilliseconds)
import Data.Maybe
import Graphics.WebGL
import Helper
import Prelude
import Shader

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

        buf <- makeBufferFloat [
            0.0, 1.0, 0.0,
            (-1.0), (-1.0), 0.0,
            1.0, (-1.0), 0.0
        ]

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

update :: forall eff. State -> EffWebGL (now :: Now | eff) State
update state = do
    timeNow <- liftM1 (unpackMilliseconds <<< toEpochMilliseconds) now
    return state {lastTime = Just timeNow}

