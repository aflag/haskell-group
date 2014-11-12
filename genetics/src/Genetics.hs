module Genetics where

import Graphics.Gloss
import Data.Array

step _ time model = model

data Square = Open | Closed | Goal

data World = World {
    antPosition :: (Int, Int),
    grid :: Array (Int, Int) Square
}

worldToPicture :: World -> Picture
worldToPicture (World antPosition g) = pictures (map positionToPicture (assocs g))
  where
   positionToPicture ((x, y), v) = translate ((fromIntegral x)*20) ((fromIntegral y)*20) (color (squareToColor v) (rectangleSolid 20 20))
   squareToColor (Open) = white
   squareToColor (Closed) = black
   squareToColor (Goal) = blue

window = InWindow "Genetics" (600, 600) (100, 100)
