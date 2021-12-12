module Main where

import Day11


main :: IO ()
main =
  do (x, y) <- Day11.solve "data/day11_input.txt"
     putStrLn $ "Day11 (part 1): " ++ (show x)
     putStrLn $ "Day11 (part 2): " ++ (show y)
