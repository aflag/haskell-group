module Main where

import Genetics
import Graphics.Gloss
import Data.Array


model = World
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

main = simulate window magenta 1 model worldToPicture step
