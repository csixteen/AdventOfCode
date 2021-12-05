module Main where

import Day1
import Day2
import Day3
import Day4

main :: IO ()
main =
  do x <- Day1.solve "data/day1_input.txt" 1
     putStrLn $ "Day1 (part 1): " ++ (show x)
     x <- Day1.solve "data/day1_input.txt" 3
     putStrLn $ "Day1 (part 2): " ++ (show x)
     x <- Day2.solve "data/day2_input.txt" Day2.move_depth
     putStrLn $ "Day2 (part 1): " ++ (show x)
     x <- Day2.solve "data/day2_input.txt" Day2.move_aim
     putStrLn $ "Day2 (part 2): " ++ (show x)
     (x, y) <- Day3.solve "data/day3_input.txt"
     putStrLn $ "Day3 (part 1): " ++ (show x)
     putStrLn $ "Day3 (part 2): " ++ (show y)
     (x, y) <- Day4.solve "data/day4_input.txt"
     putStrLn $ "Day4 (part 1): " ++ (show x)
     putStrLn $ "Day4 (part 2): " ++ (show y)
