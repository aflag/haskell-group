module Genetics where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Array
import System.Random

type Grid = Array (Int, Int) Square

data Square = Open | Closed | Goal | Start

data Movement = GoLeft | GoRight | GoUp | GoDown deriving (Enum, Bounded, Show)

data Chicken = Chicken (Int, Int) [Movement] [Movement] deriving (Show)

data World = World [Chicken] Grid (Int, Int)

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

fitness :: World -> Chicken -> Float
fitness (World _ _ goal@(x, y)) (Chicken position@(x', y') _ _) = -(sqrt $ fromIntegral radical)
  where radical = (x - x')^2 + (y - y')^2

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
move (World chickens grid goal) = World (map (moveChicken grid) chickens) grid goal

step :: ViewPort -> Float -> World -> World
step _ _ world = move world

worldToPicture :: World -> Picture
worldToPicture (World chickens g _) = pictures (worldPictures ++ chickenPictures)
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

xSize :: Int
xSize = 8

ySize :: Int
ySize = 12

maxX = xSize `div` 2
minX = -xSize `div` 2
maxY = ySize `div` 2
minY = -ySize `div` 2

world = World
    (makeChickens 10 20 (0,minY))
    (array
      ((minX,minY), (maxX,maxY))
      [if x==0 && y==maxY then ((0,maxY), Goal) else ((x, y), Open) | x <- [minX..maxX], y <- [minY..maxY]]
    )
