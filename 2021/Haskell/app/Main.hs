module Main where

import Day17


main :: IO ()
main =
  do (x, y) <- Day17.solve
     putStrLn $ "Day17 (part 1): " ++ (show x)
     putStrLn $ "Day17 (part 2): " ++ (show y)
