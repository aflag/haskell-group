module Genetics where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Array
import System.Random

type Grid = Array (Int, Int) Square

data Square = Open | Closed | Goal | Start

data Movement = GoLeft | GoRight | GoUp | GoDown deriving (Enum, Bounded, Show)

data Chicken = Chicken (Int, Int) [Movement] [Movement] deriving (Show)

data World = World [Chicken] Grid

instance Random Movement where
    random g = randomR (minBound, maxBound) g
    randomR (a,b) g =
      let (r, g') = randomR (fromEnum a, fromEnum b) g in (toEnum r, g')

chicken :: (Int, Int) -> [Movement] -> Chicken
chicken pos dna = Chicken pos dna []

makeChickens :: Int -> Int -> (Int, Int) -> [Chicken]
makeChickens 0 _ _ = []
makeChickens numChickens sizeDna pos =
    (chicken pos makeMovements) : makeChickens (numChickens-1) sizeDna pos
  where makeMovements = take sizeDna (drop (numChickens * sizeDna) (randoms (mkStdGen 13)))

nextPos :: Grid -> Movement -> (Int, Int) -> (Int, Int)
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

step :: ViewPort -> Float -> World -> World
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

world = World
    (makeChickens 10 13 (-1,-2))
    (array
      ((-1,-2), (1,2))
      [
        ((-1,-2), Open),
        ((0,-2), Open),
        ((1,-2), Open),

        ((-1,-1), Open),
        ((0,-1), Open),
        ((1,-1), Open),

        ((-1,0), Open),
        ((0,0), Open),
        ((1,0), Open),

        ((-1,1), Open),
        ((0,1), Open),
        ((1,1), Open),

        ((-1, 2), Open),
        ((0, 2), Goal),
        ((1, 2), Open)
      ]
    )
