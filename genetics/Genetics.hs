module Genetics where

import Graphics.Gloss

step _ time model = 5*time + model

window = InWindow "Genetics" (800, 640) (100, 100)
