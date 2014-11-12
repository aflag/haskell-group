module Main where

import Genetics
import Graphics.Gloss
import Data.Array


model = World (0,0) (array ((-1,-1), (1,1)) [((-1,-1), Closed), ((-1, 0), Goal), ((-1, 1), Open), ((0,-1), Closed), ((0,0), Open), ((0,1), Closed), ((1,-1), Open), ((1,0), Open), ((1,1), Closed)])

main = simulate window white 10 model worldToPicture step
