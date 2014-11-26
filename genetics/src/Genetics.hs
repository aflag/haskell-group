module Genetics where

import Graphics.Gloss
import Data.Array

step _ _ (World chickens g) = World (map (\(x,y)->(x-1, y)) chickens) g

data Square = Open | Closed | Goal | Start

type Grid = Array (Int, Int) Square
type Chicken = (Int, Int)

data World = World [Chicken] Grid

worldToPicture :: World -> Picture
worldToPicture (World chickens g) = pictures (worldPictures ++ chickenPictures)
  where
    chickenPictures = map chickenToPicture chickens
    chickenToPicture (x, y) =
      translate ((fromIntegral x)*20) ((fromIntegral y)*20) $
      color chartreuse $
      circleSolid 5
    worldPictures = map cellToPicture (assocs g)
    cellToPicture ((x, y), v) =
      translate ((fromIntegral x)*20) ((fromIntegral y)*20) $
      color (squareToColor v) $
      rectangleSolid 20 20
    squareToColor Open = white
    squareToColor Closed = black
    squareToColor Goal = blue
    squareToColor Start = green

window = InWindow "Genetics" (600, 600) (100, 100)
