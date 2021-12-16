module Main where

import Day14


main :: IO ()
main =
  do (x, y) <- Day14.solve "data/day14_input.txt"
     putStrLn $ "Day14 (part 1): " ++ (show x)
     putStrLn $ "Day14 (part 2): " ++ (show y)
