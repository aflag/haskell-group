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
       ]
   ]

main :: IO ()
main = $defaultMainGenerator
