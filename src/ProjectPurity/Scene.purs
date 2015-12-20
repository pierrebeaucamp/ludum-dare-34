module ProjectPurity.Scene (newScene, Scene()) where

import Graphics.Canvas (Canvas())
import Prelude (return)

newtype Scene = Scene {}

newScene :: forall eff. Eff (canvas :: Canvas | eff) Scene
newScene = return(Scene{})
