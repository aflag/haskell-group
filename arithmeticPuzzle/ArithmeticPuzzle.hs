-- http://www.haskell.org/haskellwiki/99_questions/90_to_94
-- Problem 93 (An arithmetic puzzle)
module ArithmeticPuzzle where

import Data.List

data Operator = Addition | Division | Multiplication | Subtraction deriving (Eq, Ord)
data BinaryTree = Leaf Float | Node Operator BinaryTree BinaryTree deriving Ord

instance Eq BinaryTree where
    Leaf x == Leaf y = x == y
    Node _ _ _ == Leaf _ = False
    Leaf _ == Node _ _ _ = False
    Node x1 y1 z1 == Node x2 y2 z2 = x1 == x2 && y1 == y2 && z1 == z2

instance Show Operator where
    show Addition = "+"
    show Division = "/"
    show Multiplication = "*"
    show Subtraction = "-"

instance Show BinaryTree where
    show (Leaf x) = show $ truncate x
    show (Node op x y) = "(" ++ show x ++ show op ++ show y ++ ")"

eval :: BinaryTree -> Float
eval (Leaf x) = x
eval (Node Addition x y) = eval x + eval y
eval (Node Division x y) = eval x / eval y
eval (Node Multiplication x y) = eval x * eval y
eval (Node Subtraction x y) = eval x - eval y

split :: [Float] -> [([Float], [Float])]
split l
    | length l < 2 = error "NullPointerException"
    | otherwise = map (flip splitAt $ l) [1..length l - 1]

treeGenerator :: [Float] -> [BinaryTree]
treeGenerator [x] = [Leaf x]
treeGenerator list = do
    (listA, listB) <- split list
    treeA <- treeGenerator listA
    treeB <- treeGenerator listB
    op <- [Addition, Subtraction]
    return (Node op treeA treeB)

puzzle :: [Float] -> [String]
puzzle numbers = filter (/="") solutions
    where
        solutions = do
            (left, right) <- split numbers
            treeLeft <- treeGenerator left
            treeRight <- treeGenerator right
            if eval treeLeft == eval treeRight
                then return (show treeLeft ++ "=" ++ show treeRight)
                else return ""
