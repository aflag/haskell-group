module Genetics where

import Graphics.Gloss
import Data.Array

type Grid = Array (Int, Int) Square

data Square = Open | Closed | Goal | Start

data Movement = GoLeft | GoRight | GoUp | GoDown

data Chicken = Chicken (Int, Int) [Movement] [Movement]

data World = World [Chicken] Grid

chicken pos dna = Chicken pos dna []

nextPos grid GoLeft (x,y)
    | inRange (bounds grid) (x-1,y) = (x-1,y)
    | otherwise = (x,y)
nextPos grid GoRight (x,y)
    | inRange (bounds grid) (x+1,y) = (x+1,y)
    | otherwise = (x,y)
nextPos grid GoUp (x,y)
    | inRange (bounds grid) (x,y+1) = (x,y+1)
    | otherwise = (x,y)
nextPos grid GoDown (x,y)
    | inRange (bounds grid) (x,y-1) = (x,y-1)
    | otherwise = (x,y)

moveChicken :: Grid -> Chicken -> Chicken
moveChicken grid (Chicken pos (m:ms) moved) = Chicken (nextPos grid m pos) ms (moved ++ [m])
moveChicken _ chicken = chicken

move :: World -> World
move (World chickens grid) = World (map (moveChicken grid) chickens) grid

step _ _ world = move world

worldToPicture :: World -> Picture
worldToPicture (World chickens g) = pictures (worldPictures ++ chickenPictures)
  where
    chickenPictures = map chickenToPicture chickens
    chickenToPicture (Chicken (x, y) _ _) =
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
