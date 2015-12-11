module Main where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Graphics.WebGLAll (runWebGL)
import Prelude (Unit())

main :: Eff (console :: CONSOLE) Unit
main = runWebGL "glcanvas" (\s -> log s) \ context -> do
    log "Canvas initialized"
