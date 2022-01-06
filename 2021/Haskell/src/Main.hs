module Main where

import AOC.Day20


main :: IO ()
main =
  do (x, y) <- solve
     putStrLn $ "Day20 (part 1): " ++ (show x)
     putStrLn $ "Day20 (part 2): " ++ (show y)
