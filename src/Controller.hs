-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Set
import Data.Array
import Data.Maybe

-- | Handle one iteration of the game
step :: Float -> Model -> IO Model
step secs (Model gState input)
  | elapsedTime gState + secs > amountSecondsBetweenStep
  =
    do return

        (Model (handleCollisions (gState {elapsedTime = 0, pacman = advancePacman secs (maze gState) (pacman gState)})) (processInputKeys input))
  | otherwise
  = -- Just update the elapsed time
    return (Model gState { elapsedTime = elapsedTime gState + secs } input)


advancePacman :: Float ->  Maze -> Pacman -> Pacman
advancePacman delta maze pacman =
  let 
      --add a check to reset direction back to original after a very short time?????????
      --to create a short window where turn move gets checked and used and not always, think that should be done???
      movement = pSpeed pacman * delta
      turningCell = nextCellInDirection (pCell pacman) (pNext pacman)

      centered = if isAtCellCenter pacman movement
                 then pacman {pStepsX = 0, pStepsY = 0}
                 else pacman

      startingReverse = isOpposite (pDir centered) (pNext pacman) 
      atCenter = pStepsX centered == 0 && pStepsY centered == 0 
      canTurn = startingReverse || (atCenter && not (isWall turningCell maze))
      targetDir = if canTurn then pNext centered else pDir centered

      stillReversing = (targetDir == U && pStepsY centered > 0) ||
                       (targetDir == D && pStepsY centered < 0) ||
                       (targetDir == L && pStepsX centered > 0) ||
                       (targetDir == R && pStepsX centered < 0)

      isReversing = startingReverse || (pReversing centered && stillReversing)

      snappedPacman = if targetDir /= pDir centered && not startingReverse
                      then case targetDir of
                        U -> pacman {pStepsX = 0}
                        D -> pacman {pStepsX = 0}
                        L -> pacman {pStepsY = 0}
                        R -> pacman {pStepsY = 0}
                      else pacman

      nextCell = nextCellInDirection (pCell snappedPacman) targetDir

  in if not (isWall nextCell maze) || isReversing
     then moveInDirection (snappedPacman {pReversing = isReversing}) nextCell movement targetDir
     else pacman {pStepsX = 0, pStepsY = 0, pDir = targetDir, pReversing = False}

isAtCellCenter :: Pacman -> Float -> Bool
isAtCellCenter pac mov = 
  abs (pStepsX pac) <= mov && abs (pStepsY pac) <= mov

isOpposite :: Direction -> Direction -> Bool
isOpposite U D = True
isOpposite D U = True
isOpposite L R = True
isOpposite R L = True
isOpposite _ _ = False

moveInDirection :: Pacman -> Cell -> Float -> Direction -> Pacman
moveInDirection pacman cell mov dir = case dir of
  U -> let newpStepsY = pStepsY pacman + mov
       in if newpStepsY >= tileSize
          then pacman {pCell = cell,
                       pStepsY = newpStepsY - tileSize,
                       pDir = dir}
          else pacman {pStepsY = newpStepsY, pDir = dir}
  D -> let newpStepsY = pStepsY pacman - mov
       in if newpStepsY <= -tileSize
          then pacman {pCell = cell,
                       pStepsY = newpStepsY + tileSize,
                       pDir = dir }
          else pacman {pStepsY = newpStepsY, pDir = dir }
  L -> let newpStepsX = pStepsX pacman - mov
       in if newpStepsX <= -tileSize
          then pacman {pCell = cell,
                       pStepsX = newpStepsX + tileSize,
                       pDir = dir}
          else pacman {pStepsX = newpStepsX, pDir = dir}
  R -> let newpStepsX = pStepsX pacman + mov
       in if newpStepsX >= tileSize
          then pacman {pCell = cell,
                       pStepsX = newpStepsX - tileSize,
                       pDir = dir}
          else pacman {pStepsX = newpStepsX, pDir = dir}

nextCellInDirection :: Cell -> Direction -> Cell
nextCellInDirection (x, y) U = (x, y - 1)
nextCellInDirection (x, y) D = (x, y + 1)
nextCellInDirection (x, y) L = (x - 1, y)
nextCellInDirection (x, y) R = (x + 1, y)

isWall :: Cell -> Maze -> Bool
isWall cell maze = not (inRange gridBounds cell) || ((maze ! cell) == Wall)

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