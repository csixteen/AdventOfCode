module Main where

import Day13


main :: IO ()
main =
  do (x, y) <- Day13.solve "data/day13_input.txt"
     putStrLn $ "Day13 (part 1): " ++ (show x)
     putStrLn $ "Day13 (part 2): " ++ (show y)
