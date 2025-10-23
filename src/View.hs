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
      (offsetX + fromIntegral x * tileSize,
       offsetY - fromIntegral y * tileSize)

-- Just walls for now
drawTile :: ( (Int,Int), Tile ) -> Picture
drawTile ((x,y), t) =
  case t of
    Wall ->
      let (cx, cy) = cellCenter (x,y)
      in translate cx cy $ color (makeColorI 33 33 255 255) (rectangleSolid (tileSize-2) (tileSize-2))
    Pellet ->
      let (cx, cy) = cellCenter (x,y)
      in translate cx cy $ color (makeColorI 255 255 255 255) (circleSolid 1)
    PowerPellet ->
      let (cx, cy) = cellCenter (x,y)
      in translate cx cy $ color (makeColorI 255 255 0 255) (circleSolid 2)
    Gate ->
      let (cx, cy) = cellCenter (x,y)
      in translate cx cy $ color (makeColorI 255 0 255 255) (rectangleSolid tileSize (tileSize / 4))
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
drawPlayer pacman = let (cx, cy) = cellCenter (pCell pacman)
                    in translate cx cy $ color (makeColorI 255 255 0 225) (ThickCircle 6 5)

drawPacman :: Pacman -> Picture
drawPacman pac =
  let (cx, cy) = cellCenter (pCell pac)
  in translate cx cy $ color yellow (circleSolid (tileSize / 2))

scene :: GameState -> Picture
scene gs = Pictures
  [ color black (rectangleSolid screenX screenY)
  , drawMaze (maze gs)
  , drawPacman (pacman gs)
  ]

view :: Model -> IO Picture
view = return . viewPure

viewPure :: Model -> Picture
viewPure m = scene (gState m)

-- viewPure (Model gState input) = Pictures [Translate (-300) 80 (color green (text (show (char1 (chars input))))),
--                                           Translate (-200) 80 (color green (text (show (char2 (chars input))))),
--                                           Translate (-100) 80 (color green (text (show (char3 (chars input))))),
--                                           Translate 0      80 (color green (text (show (char4 (chars input))))),
--                                           Translate 100    80 (color green (text (show (pCell (pacman gState)))))]