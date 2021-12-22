module Main where

import Day18


main :: IO ()
main =
  do (x, y) <- Day18.solve
     putStrLn $ "Day18 (part 1): " ++ (show x)
     putStrLn $ "Day18 (part 2): " ++ (show y)
