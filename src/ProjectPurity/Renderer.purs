module ProjectPurity.Renderer (
    newRenderer,
    Renderer(),
    requestAnimationFrame
) where

import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..))
import DOM (DOM())
import qualified DOM.RequestAnimationFrame as D
import Graphics.Canvas (Canvas(), getCanvasElementById)
import Graphics.WebGL.Context
import Graphics.WebGL.Raw (clearColor)
import Graphics.WebGL.Raw.Types (WebGLContext())
import Prelude (bind, return, Unit())
import ProjectPurity.Scene

newtype Renderer = Renderer {
    context :: WebGLContext
}

newRenderer :: forall eff. String -> Eff (canvas :: Canvas | eff) Renderer
newRenderer canvasId = do
    Just canvas <- getCanvasElementById canvasId
    context <- createContext canvas defaultContextAttributes
    return(Renderer{context: context})

render :: forall eff. Renderer -> Scene -> Eff (canvas :: Canvas | eff) Unit
render (Renderer renderer) (Scene scene) = do
    clearColor renderer.context 0.29 0.27 0.33 1.0


requestAnimationFrame :: forall a eff.
                             Eff (dom :: DOM | eff) a ->
                             Eff (dom :: DOM | eff) Unit
requestAnimationFrame = D.requestAnimationFrame
