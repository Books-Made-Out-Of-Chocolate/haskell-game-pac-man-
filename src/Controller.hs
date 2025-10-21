-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Set
import Data.Array

-- | Handle one iteration of the game
step :: Float -> Model -> IO Model
step secs (Model gstate input)
  | elapsedTime gstate + secs > amountSecondsBetweenStep
  = -- We show a new random number
    do return (Model gstate input)
  | otherwise
  = -- Just update the elapsed time
    return (Model gstate { elapsedTime = elapsedTime gstate + secs } input)


advancePacman ::  Maze -> Pacman -> Pacman
advancePacman maze pacman = pacman
--ghostStep :: Maze -> StdGen -> Pacman -> Ghost -> (Ghost, StdGen)

-- | Handle user input
input :: Event -> Model -> IO Model
input e model = return (inputKey e model)

inputKey :: Event -> Model -> Model
inputKey (EventKey (Char c) _ _ _)  (Model gState (InputControls keys chars)) = Model gState (InputControls  keys (c : tail chars))
inputKey _ input = input