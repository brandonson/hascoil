module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Game

main::IO()
main = do
        SDL.init [SDL.InitEverything]
        videoSurface <- SDL.setVideoMode 640 640 32 []
        SDL.setCaption "Hascoil" "Hascoil"
        start <- Game.startState
        Game.eventLoop start videoSurface
        SDL.quit
