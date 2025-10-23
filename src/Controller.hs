-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Set
import Data.Array

-- | Handle one iteration of the game
step :: Float -> Model -> IO Model
step secs (Model gState input)
  | elapsedTime gState + secs > amountSecondsBetweenStep
  = -- We show a new random number
    do return

        (Model (handleCollisions (gState {elapsedTime = 0, pacman = advancePacman (maze gState) (pacman gState)})) (processInputKeys input))
  | otherwise
  = -- Just update the elapsed time
    return (Model gState { elapsedTime = elapsedTime gState + secs } input)


advancePacman ::  Maze -> Pacman -> Pacman
advancePacman maze pacman =
      let pos = pCell pacman
          wantDir = pNext pacman
          curDir = pDir pacman
          passable cell = (maze ! cell) /= Wall
          step = nextCell pos
          canTurn = passable (step wantDir)
          canMove = passable (step curDir)
          dir = if canTurn then wantDir else curDir
          nextPos = nextCell pos dir
      in if canTurn || canMove
       then pacman { pCell = nextPos, pDir = dir }
       else pacman

removeItem :: Cell -> Maze -> Maze
removeItem cell maze = maze // [(cell, Empty)]

cellType :: Cell -> Maze  -> Tile
cellType cell maze = maze ! cell

handleCollisions :: GameState -> GameState
handleCollisions gState =
  let pacCell = pCell (pacman gState)
      curMaze = maze gState
      curScore = score gState
      curCellType = cellType pacCell curMaze
  in case curCellType of
        Pellet ->
          gState { maze = removeItem pacCell curMaze,
                    score = curScore + 10 }
        PowerPellet ->
          gState { maze = removeItem pacCell curMaze,
                    score = curScore + 50 }
        _ -> gState


--ghostStep :: Maze -> StdGen -> Pacman -> Ghost -> (Ghost, StdGen)

nextCell :: Cell -> Direction -> Cell
nextCell (x, y) U = (x, y - 1)
nextCell (x, y) D = (x, y + 1)
nextCell (x, y) L = (x - 1, y)
nextCell (x, y) R = (x + 1, y)

--temp function
processInputKeys :: InputControls -> InputControls
processInputKeys input@(InputControls keys chars) = case (pause keys, reset keys, enter keys) of
                                          (True, _, _) -> input
                                          (_, True, _) -> input
                                          (_, _, True) -> input
                                          (_, _, _   ) -> input

-- | Handle user input
input :: Event -> Model -> IO Model
input e model = return (inputKey e model)

inputKey :: Event -> Model -> Model
--up arrow key pressed
inputKey (EventKey (SpecialKey KeyUp) Down _ _)  (Model gState inputs) =
            Model gState {pacman = (pacman gState) {pNext = U}}
                  inputs
--down arrow key pressed
inputKey (EventKey (SpecialKey KeyDown) Down _ _)  (Model gState inputs) =
            Model gState {pacman = (pacman gState) {pNext = D}}
                  inputs
--left arrow key pressed
inputKey (EventKey (SpecialKey KeyLeft) Down _ _)  (Model gState inputs) =
            Model gState {pacman = (pacman gState) {pNext = L}}
                  inputs
--right arrow key pressed
inputKey (EventKey (SpecialKey KeyRight) Down _ _)  (Model gState inputs) =
            Model gState {pacman = (pacman gState) {pNext = R}}
                  inputs
--escape key pressed
inputKey (EventKey (SpecialKey KeyEsc) Down _ _)  (Model gState (InputControls keys chars)) =
            Model gState
                  (InputControls (keys {pause = True}) chars)
--home key pressed
inputKey (EventKey (SpecialKey KeyHome) Down _ _)  (Model gState (InputControls keys chars)) =
            Model gState
                  (InputControls (keys {reset = True}) chars)
--enter key pressed
inputKey (EventKey (SpecialKey KeyEnter) Down _ _)  (Model gState (InputControls keys chars)) =
            Model gState
                  (InputControls (keys {enter = True}) chars)
--backspace key pressed, immediately remove a char from CharsHighScoreInput
inputKey (EventKey (Char '\b') Down _ _)  (Model gState (InputControls keys chars)) =
            Model gState
                  (InputControls keys (removeCharInOrder chars))
--highscore name input
inputKey (EventKey (Char c) Down _ _)  model@(Model gState (InputControls keys chars)) =
            Model gState
                  (InputControls keys (placeCharInOrder chars c))
--default, return model
inputKey _ model = model

placeCharInOrder :: CharsHighScoreInput -> Char -> CharsHighScoreInput
placeCharInOrder chars c = case (char1 chars, char2 chars, char3 chars, char4 chars) of
                           (' ', ' ', ' ', ' ') -> chars {char1 = c}
                           (_  , ' ', ' ', ' ') -> chars {char2 = c}
                           (_  , _  , ' ', ' ') -> chars {char3 = c}
                           (_  , _  , _  , ' ') -> chars {char4 = c}
                           (_  , _  , _  , _  ) -> chars

removeCharInOrder :: CharsHighScoreInput -> CharsHighScoreInput
removeCharInOrder chars = case (char1 chars, char2 chars, char3 chars, char4 chars) of
                           (' ', ' ', ' ', ' ') -> chars
                           (_  , ' ', ' ', ' ') -> chars {char1 = ' '}
                           (_  ,  _ , ' ', ' ') -> chars {char2 = ' '}
                           (_  ,  _ ,  _ , ' ') -> chars {char3 = ' '}
                           (_  ,  _ ,  _ ,  _ ) -> chars {char4 = ' '}