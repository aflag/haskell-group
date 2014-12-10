module Main where

import Genetics
import Graphics.Gloss
import Data.Array

main = simulate window magenta 1 world worldToPicture step
