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

makeMovements :: StdGen -> Int -> ([Movement], StdGen)
makeMovements gen 0 = ([], gen)
makeMovements gen size = (newMovement:movements, gen'')
  where
    (newMovement, gen') = random gen
    (movements, gen'') = makeMovements gen' (size-1)

makeChickens :: StdGen -> Int -> Int -> (Int, Int) -> ([Chicken], StdGen)
makeChickens gen 0 _ _ = ([], gen)
makeChickens gen numChickens sizeDna pos
  = ((chicken pos movements) : chickens, gen'')
  where
    (movements, gen') = makeMovements gen sizeDna
    (chickens, gen'') = makeChickens gen' (numChickens-1) sizeDna pos

fitness :: World -> Chicken -> Float
fitness (World _ _ goal@(x, y)) (Chicken position@(x', y') _ _) = -(sqrt $ fromIntegral radical)
  where radical = (x - x')^2 + (y - y')^2

-- crossover :: Chicken -> Chicken -> [Chicken]
-- mutation

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

world = World chickens grid goal
  where
    goal = (0, maxY)
    (chickens, _) = makeChickens (mkStdGen 3) 10 20 (0,minY)
    grid = (array
      ((minX,minY), (maxX,maxY))
      [if x==0 && y==maxY then (goal, Goal) else ((x, y), Open) | x <- [minX..maxX], y <- [minY..maxY]])
