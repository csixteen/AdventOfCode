module Main where

import Day9


main :: IO ()
main =
  do (x, y) <- Day9.solve "data/day9_input.txt"
     putStrLn $ "Day9 (part 1): " ++ (show x)
     putStrLn $ "Day9 (part 2): " ++ (show y)
