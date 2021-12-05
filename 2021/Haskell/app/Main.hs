module Main where

import Day5

main :: IO ()
main =
  do x <- Day5.solve "data/day5_input.txt"
     putStrLn $ "Day5 (part 1): " ++ (show x)
