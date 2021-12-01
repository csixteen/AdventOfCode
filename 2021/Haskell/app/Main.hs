module Main where

import Day1

main :: IO ()
main =
  do x <- Day1.solve "data/day1_input.txt" Day1.larger_than
     putStrLn $ "Day1 (part 1): " ++ (show x)
     x <- Day1.solve "data/day1_input.txt" Day1.larger_than_sliding
     putStrLn $ "Day1 (part 2): " ++ (show x)
