-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: Model -> IO Picture
view = return . viewPure

viewPure :: Model -> Picture
viewPure (Model gState input) = Pictures [Translate (-300) 80 (color green (text (show (char1 (chars input))))),
                                          Translate (-200) 80 (color green (text (show (char2 (chars input))))),
                                          Translate (-100) 80 (color green (text (show (char3 (chars input))))),
                                          Translate 0      80 (color green (text (show (char4 (chars input))))),
                                          Translate 100    80 (color green (text (show (pCell (pacman gState)))))]