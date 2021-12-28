module Main where

import AOC.Day19


main :: IO ()
main =
  do (x, y) <- solve
     putStrLn $ "Day19 (part 1): " ++ (show x)
     putStrLn $ "Day19 (part 2): " ++ (show y)
