{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH
import Test.Tasty.HUnit
import Data.List
import ArithmeticPuzzle


prop_size_is_always_one_less x = (length x > 1) ==> length (split x) == (length x - 1)

prop_joint_response_combinador x = (length x > 1) ==> all (x==) joint_lists
    where joint_lists = map (uncurry (++)) (split x)

test_unit = [
       testGroup "Test puzzle" [
           testCase "one solution" (assertEqual "" (puzzle [1,2,3]) (["(1+2)=3"])),
           testCase "two solution" (assertEqual "" (sort $ puzzle [6,4,2]) (sort ["6=(4+2)", "(6-4)=2"])),
           testCase "two solutions (tati)" (assertEqual "simple" (sort $ puzzle [2,2,0]) (sort ["(2-2)=0", "2=(2+0)", "2=(2-0)"])),
           testCase "four solutions (senra)" (assertEqual "simple" (puzzle [1,2,3,6]) (["(1+(2+3))=6", "(1*(2*3))=6", "1=(2*3)/6", "1/2=3/6"]))
       ],
       testGroup "Test given a list create a tree (tati)" [
            testCase "One element" (assertEqual "" (treeGenerator [1]) [Leaf 1]),
            testCase "Two elements" (assertEqual "" (treeGenerator [1,2]) [Node Addition (Leaf 1) (Leaf 2)]),
            testCase "Three elements" (assertEqual "" (sort $ treeGenerator [1,2,3]) (sort [
                Node Addition (Node Addition (Leaf 1) (Leaf 2)) (Leaf 3),
                Node Addition (Leaf 1) (Node Addition (Leaf 2) (Leaf 3))
            ])),
            testCase "Four elements" (assertEqual "" (sort $ treeGenerator [1,2,3,4]) (sort [
                Node Addition (Node Addition (Leaf 1) (Leaf 2)) (Node Addition (Leaf 3) (Leaf 4)),
                Node Addition (Leaf 1) (Node Addition (Leaf 2) (Node Addition (Leaf 3) (Leaf 4))),
                Node Addition (Node Addition (Node Addition (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4),
                Node Addition (Leaf 1) (Node Addition (Node Addition (Leaf 2) (Leaf 3)) (Leaf 4)),
                Node Addition (Node Addition (Leaf 1) (Node Addition (Leaf 2) (Leaf 3))) (Leaf 4)
            ]))
       ]
   ]

main :: IO ()
main = $defaultMainGenerator
