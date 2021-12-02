module Main where

import Day1
import Day2

main :: IO ()
main =
  do x <- Day1.solve "data/day1_input.txt" 1
     putStrLn $ "Day1 (part 1): " ++ (show x)
     x <- Day1.solve "data/day1_input.txt" 3
     putStrLn $ "Day1 (part 2): " ++ (show x)
     x <- Day2.solve "data/day2_input.txt"
     putStrLn $ "Day2 (part 2): " ++ (show x)
