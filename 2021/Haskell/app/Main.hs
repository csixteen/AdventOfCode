module Main where

import Day16


main :: IO ()
main =
  do (x, y) <- Day16.solve
     putStrLn $ "Day16 (part 1): " ++ (show x)
     putStrLn $ "Day16 (part 2): " ++ (show y)
