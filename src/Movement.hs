-- | This module holds class and implementation of movement

module Movement where

import Model

import Data.Array
import System.Random

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

      stillReversing = (targetDir == U && pStepsY centered < 0) ||
                       (targetDir == D && pStepsY centered > 0) ||
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

--advanceGhost:: Ghost -> Maze -> StdGen -> Pacman -> (Ghost, StdGen)


isWall :: Cell -> Maze -> Bool
isWall cell maze = not (inRange gridBounds cell) || ((maze ! cell) == Wall)

isAtCellCenter :: Pacman -> Float -> Bool
isAtCellCenter pac mov = 
  abs (pStepsX pac) <= mov && abs (pStepsY pac) <= mov

isOpposite :: Direction -> Direction -> Bool
isOpposite U D = True
isOpposite D U = True
isOpposite L R = True
isOpposite R L = True
isOpposite _ _ = False

nextCellInDirection :: Cell -> Direction -> Cell
nextCellInDirection (x, y) U = (x, y - 1)
nextCellInDirection (x, y) D = (x, y + 1)
nextCellInDirection (x, y) L = (x - 1, y)
nextCellInDirection (x, y) R = (x + 1, y)

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