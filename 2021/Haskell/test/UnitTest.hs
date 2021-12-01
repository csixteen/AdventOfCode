module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Day1

main :: IO ()
main = defaultMain simpleTests

simpleTests :: TestTree
simpleTests = testGroup "Simple Tests"
  [
    testCase "Day 1 (part 1)" $
      Day1.larger [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] 1 @?= 7
  ]
