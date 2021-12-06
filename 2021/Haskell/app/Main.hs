module Main where

import Day6


main :: IO ()
main =
  do (x, y) <- Day6.solve "data/day6_input.txt"
     putStrLn $ "Day6 (part 1): " ++ (show x)
     putStrLn $ "Day6 (part 2): " ++ (show y)
