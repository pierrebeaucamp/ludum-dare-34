module Graphics.WebGL.Context (createContext, defaultContextAttributes) where

import Control.Monad.Eff (Eff())
import Graphics.Canvas (Canvas(), CanvasElement())
import Graphics.WebGL.Raw.Types (WebGLContext(), WebGLContextAttributes())

defaultContextAttributes :: WebGLContextAttributes
defaultContextAttributes = {
    alpha: true,
    depth: false,
    stencil: false,
    antialias: true,
    premultipliedAlpha: true,
    preserveDrawingBuffer: false,
    preferLowPowerToHighPerformance: false,
    failIfMajorPerformanceCaveat: false
}

foreign import createContext :: forall eff.
                                    CanvasElement ->
                                    WebGLContextAttributes ->
                                    Eff (canvas :: Canvas | eff) WebGLContext
