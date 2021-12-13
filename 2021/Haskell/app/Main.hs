module Main where

import Day12


main :: IO ()
main =
  do (x, y) <- Day12.solve "data/day12_input.txt"
     putStrLn $ "Day12 (part 1): " ++ (show x)
     putStrLn $ "Day12 (part 2): " ++ (show y)
