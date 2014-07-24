-- http://www.haskell.org/haskellwiki/99_questions/90_to_94
-- Problem 93 (An arithmetic puzzle)
module ArithmeticPuzzle where

ops :: [(Int -> Int -> Int)]
ops = [(+)]

check :: [Int] -> Int -> Bool
check l n = sum (take n l) == sum (drop n l)

findCut :: [Int] -> [Bool]
findCut l = map (check l) [1..length l - 1]

