module Main where

import Day7


main :: IO ()
main =
  do (x, y) <- Day7.solve "data/day7_input.txt"
     putStrLn $ "Day7 (part 1): " ++ (show x)
     putStrLn $ "Day7 (part 2): " ++ (show y)
