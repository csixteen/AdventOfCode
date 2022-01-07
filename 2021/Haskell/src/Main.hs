module Main where

import AOC.Day20


main :: IO ()
main =
  do (x, y) <- solve
     putStrLn $ "Day21 (part 1): " ++ (show x)
     putStrLn $ "Day21 (part 2): " ++ (show y)
