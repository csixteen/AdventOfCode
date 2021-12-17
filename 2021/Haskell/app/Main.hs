module Main where

import Day15


main :: IO ()
main =
  do (x, y) <- Day15.solve "data/day15_input.txt"
     putStrLn $ "Day15 (part 1): " ++ (show x)
     putStrLn $ "Day15 (part 2): " ++ (show y)
