-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import qualified Data.Array


-- Screen and tile parameters
screenW, screenH, tileSize :: Float
screenW = 224
screenH = 288
tileSize = 8

cellCenter :: Cell -> WorldPos
cellCenter (x, y) =
  let halfW = screenW  / 2
      halfH = screenH  / 2
  in (-halfW + 4 + fromIntegral x * tileSize,
       halfH - 4 - fromIntegral y * tileSize)

-- Just walls for now
drawTile :: ( (Int,Int), Tile ) -> Picture
drawTile ((x,y), t) =
  case t of
    Wall ->
      let (cx, cy) = cellCenter (x,y)
      in translate cx cy $ color (makeColorI 33 33 255 255) (rectangleSolid tileSize tileSize)
    _    -> Blank

drawMaze :: Maze -> Picture
drawMaze mz = Pictures ( map drawTile (Data.Array.assocs mz))

scene :: GameState -> Picture
scene gs = Pictures
  [ color black (rectangleSolid screenW screenH)
  , drawMaze (maze gs)
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