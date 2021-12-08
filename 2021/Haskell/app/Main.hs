module Main where

import Day8


main :: IO ()
main =
  do (x, y) <- Day8.solve "data/day8_input.txt"
     putStrLn $ "Day8 (part 1): " ++ (show x)
     putStrLn $ "Day8 (part 2): " ++ (show y)
