module ProjectPurity (
    newRenderer,
    requestAnimationFrame
) where

import Control.Monad.Eff (Eff())
import DOM (DOM())
import Graphics.Canvas (Canvas())
import Prelude (Unit())
import qualified ProjectPurity.Renderer as R

newRenderer :: forall eff. String -> Eff (canvas :: Canvas | eff) R.Renderer
newRenderer = R.newRenderer

render = R.render

requestAnimationFrame :: forall a eff.
                             Eff (dom :: DOM | eff) a ->
                             Eff (dom :: DOM | eff) Unit
requestAnimationFrame = R.requestAnimationFrame
