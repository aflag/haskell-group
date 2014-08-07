-- http://www.haskell.org/haskellwiki/99_questions/90_to_94
-- Problem 93 (An arithmetic puzzle)
module ArithmeticPuzzle where

data Operator = Addition | Division | Multiplication | Subtraction
data BinaryTree a = Leaf a | Node Operator (BinaryTree a) (BinaryTree a)

instance Show Operator where
    show Addition = "+"
    show Division = "/"
    show Multiplication = "*"
    show Subtraction = "-"

instance Show a => Show (BinaryTree a) where
    show (Leaf x) = show x
    show (Node op x y) = "(" ++ show x ++ show op ++ show y ++ ")"

eval :: (BinaryTree Float) -> Float
eval (Leaf x) = x
eval (Node Addition x y) = eval x + eval y
eval (Node Division x y) = eval x / eval y
eval (Node Multiplication x y) = eval x * eval y
eval (Node Subtraction x y) = eval x - eval y

check :: [Int] -> Int -> Bool
check l n = sum (take n l) == sum (drop n l)

findCut :: [Int] -> [Bool]
findCut l = map (check l) [1..length l - 1]

puzzle _ = ["1 + 2 = 3"]
