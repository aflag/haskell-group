{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH
import Test.Tasty.HUnit
import Data.List
import ArithmeticPuzzle


-- prop_cross_with_b x y = (x == B || y == B) ==> B `elem` cross x y

test_unit = [
       testGroup "Test puzzle" [
           testCase "one solution" (assertEqual "" (puzzle [1,2,3]) (["1 + 2 = 3"])),
           testCase "two solutions (tati)" (assertEqual "simple" (puzzle [2,2,0]) (["2 - 2 = 0", "2 = 2 + 0"])),
           testCase "four solutions (senra)" (assertEqual "simple" (puzzle [1,2,3,6]) (["1 + 2 + 3 = 6", "1 * 2 * 3 = 6", "1 = (2 * 3) / 6", "1 / 2 = 3 / 6"]))
       ],
       testGroup "Test given a list create a tree(tati)" [
            testCase "" (assertEqual "" (treeGenerator [1]) [Leaf 1])
            --testCase "" (assertEqual "" (treeGenerator [1,2]) [Node Addition (Leaf 1) (Leaf 2)]),
            --testCase "" (assertEqual "" (treeGenerator [1,2,3]) [Node Addition (Node Addition (Leaf 1) (Leaf 2)) (Leaf 3)], [Node Addition (Leaf 1 (Node Addition (Leaf 2) (Leaf 3)))])
       ]
   ]

main :: IO ()
main = $defaultMainGenerator
