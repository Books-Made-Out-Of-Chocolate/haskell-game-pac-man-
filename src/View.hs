-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Set
import qualified Data.Array


-- Screen and tile parameters
screenX, screenY, offsetX, offsetY :: Float
screenX = fromIntegral (gridMaxX + 1) * tileSize
screenY = fromIntegral (gridMaxY + 1) * tileSize
offsetX = -(screenX / 2) + (tileSize / 2)
offsetY =  (screenY / 2)  - (tileSize / 2)

cell_To_wPos :: Cell -> WorldPos
cell_To_wPos (x, y) =
      (offsetX + (fromIntegral x * tileSize),
       offsetY - (fromIntegral y * tileSize))

wPos_To_Cell :: WorldPos -> Cell
wPos_To_Cell (x, y)=
      (floor ((x - offsetX) / tileSize),
       floor ((offsetY - y) / tileSize))

-- Just walls for now
drawTile :: ( (Int,Int), Tile ) -> Picture
drawTile ((x,y), t) =
  case t of
    Wall ->
      let (cx, cy) = cell_To_wPos (x,y)
      in translate cx cy $ color (makeColorI 33 33 255 255) (rectangleSolid (tileSize-2) (tileSize-2))
    Pellet ->
      let (cx, cy) = cell_To_wPos (x,y)
      in translate cx cy $ color (makeColorI 255 255 255 255) (circleSolid 1)
    PowerPellet ->
      let (cx, cy) = cell_To_wPos (x,y)
      in translate cx cy $ color (makeColorI 255 255 0 255) (circleSolid 2)
    Gate ->
      let (cx, cy) = cell_To_wPos (x,y)
      in translate cx cy $ color (makeColorI 255 0 255 255) (rectangleSolid tileSize (tileSize / 4))
    _    -> Blank

drawMaze :: Maze -> Picture
drawMaze mz = Pictures (Prelude.map drawTile (Data.Array.assocs mz))

drawInterpolationPacman :: Pacman -> Picture
drawInterpolationPacman pac =
  let (cx, cy) = cell_To_wPos (pCell pac)
      (x, y)   = (cx + pStepsX pac, cy + pStepsY pac)
  in translate x y $ color yellow (circleSolid (tileSize / 2))

drawInterpolationGhosts :: [Ghost] -> Picture
drawInterpolationGhosts ghosts = Pictures ( Prelude.map 
                                          (\ghost -> let (cx, cy) = cell_To_wPos( gCell ghost)
                                                         (x, y)   = (cx + gStepsX ghost, cy + gStepsY ghost)
                                          in translate x y $ color orange (circleSolid (tileSize / 2))) 
                                          ghosts)

scene :: GameState -> Picture
scene gs = Pictures
  [
    color black (rectangleSolid screenX screenY),
    drawMaze (maze gs),
    drawInterpolationPacman (pacman gs),
    drawInterpolationGhosts (ghosts gs),
    color green (text (show (pStepsX (pacman gs), pStepsY (pacman gs)))),
    translate 0 (-100) $ color green (text (show (pCell (pacman gs)))),
    translate (-300) (-80) $ color green (text (show (pNext (pacman gs)))),
    translate (-200) (-180) $ color green (text (show (gameOver gs)))
  ]

view :: Model -> IO Picture
view = return . viewPure

viewPure :: Model -> Picture
viewPure m = scene (gState m)