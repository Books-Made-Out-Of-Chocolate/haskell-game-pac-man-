module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Pac-Man" (round screenW, round screenH) (0,0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialModel     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

