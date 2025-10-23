-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Set
import qualified Data.Array


-- Screen and tile parameters
screenX, screenY, tileSize, offsetX, offsetY :: Float
screenX = fromIntegral (gridMaxX + 1) * tileSize
screenY = fromIntegral (gridMaxY + 1) * tileSize
tileSize = 10
offsetX = -(screenX / 2) + tileSize / 2
offsetY =   screenY / 2  - tileSize / 2

cellCenter :: Cell -> WorldPos
cellCenter (x, y) = 
      (offsetX + (fromIntegral x * tileSize),
       offsetY - (fromIntegral y * tileSize))

worldPosToCell :: WorldPos -> Cell
worldPosToCell (x, y)=
      (floor(x / tileSize),
       floor(y / tileSize))

worldPosToScreenPos :: WorldPos -> (Float, Float)
worldPosToScreenPos (x, y) =
      (offsetX + x,
      offsetY - y)

-- Just walls for now
drawTile :: ( (Int,Int), Tile ) -> Picture
drawTile ((x,y), t) =
  case t of
    Wall ->
      let (cx, cy) = cellCenter (x,y)
      in translate cx cy $ color (makeColorI 33 33 255 255) (rectangleSolid tileSize tileSize)
    _    -> Blank

drawMaze :: Maze -> Picture
drawMaze mz = Pictures (Prelude.map drawTile (Data.Array.assocs mz))

drawDot :: Cell -> Picture
drawDot cell = let (cx, cy) = cellCenter cell
                  in translate cx cy $ color (makeColorI 0 255 0 255) (Circle 3)

drawPowerDots :: Cell -> Picture
drawPowerDots cell = let (cx, cy) = cellCenter cell
                  in translate cx cy $ color (makeColorI 65 100 0 255) (Circle 5)

drawPellets :: Pellets -> Picture
drawPellets pellets = Pictures (Prelude.map drawDot (toList (dots pellets)) ++ Prelude.map drawPowerDots (toList (powerDots pellets)))

drawPlayer :: Pacman -> Picture
drawPlayer pacman = let (cx, cy) = worldPosToScreenPos (pPos pacman)
                    in translate cx cy $ color (makeColorI 255 255 0 225) (ThickCircle 6 5)

scene :: GameState -> Picture
scene gs = Pictures
  [ 
    color black (rectangleSolid screenX screenY),
    drawMaze (maze gs),
    drawPellets (pellets gs),
    drawPlayer (pacman gs),
    color green (text (show (pPos (pacman gs) ))),
    translate 0 (-100) $ color green (text (show (pCell (pacman gs)))),
    translate (-300) (-80) $ color green (text (show (pNext (pacman gs))))
  ]

view :: Model -> IO Picture
view = return . viewPure

viewPure :: Model -> Picture
viewPure m = scene (gState m)