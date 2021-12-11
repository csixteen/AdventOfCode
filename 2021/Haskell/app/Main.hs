module Main where

import Day10


main :: IO ()
main =
  do (x, y) <- Day10.solve "data/day10_sample.txt"
     putStrLn $ "Day10 (part 1): " ++ (show x)
     putStrLn $ "Day10 (part 2): " ++ (show y)
