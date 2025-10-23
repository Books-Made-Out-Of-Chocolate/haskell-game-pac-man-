module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  initial <- initialModel
  playIO (InWindow "Pac-Man" (round screenX, round screenY) (0,0)) -- Or FullScreen
         black            -- Background color
         100               -- Frames per second
         initial          -- Initial state
         view             -- View function
         input            -- Event function
         step             -- Step function

