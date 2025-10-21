-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Set
import Data.Array
import Data.Char

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState 
                        (array ((1, 2), (1, 3)) [((1, 2), Wall)]) 
                        (Pellets (Data.Set.fromList [(1,1),(1,2)]) Data.Set.empty)
                        (Pacman (1.2, 1.2) U (Speed 3.2) D)
                        [Ghost (1.2, 1.2) U Chase (Speed 3.2), Ghost (1.2, 1.2) U Chase (Speed 3.2)]
                        randomNumber
                        12
                        54
                        False
                        (mkStdGen 6)
                        (UIState [("wqe", 23), ("2vwe", 32)] (Just "qqwe"))
                        0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { score = ord c }
inputKey _ gstate = gstate -- Otherwise keep the same