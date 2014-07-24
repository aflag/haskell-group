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

-- test_unit = [
--        testGroup "Unit" [
--            testCase "Cross AB AB" (assertEqual "" (sort [AB, B, A]) (sort $ cross AB AB))
--        ]
--    ]

main :: IO ()
main = $defaultMainGenerator

