-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: Model -> IO Picture
view = return . viewPure

viewPure :: Model -> Picture
viewPure (Model gstate input) = color green (text (show (head (chars input))))