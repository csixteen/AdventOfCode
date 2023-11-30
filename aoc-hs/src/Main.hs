module Main where

import AOC.Day22 (solve)


main :: IO ()
main =
  do (x, y) <- solve
     putStrLn $ "Day22 (part 1): " ++ (show x)
     putStrLn $ "Day22 (part 2): " ++ (show y)
