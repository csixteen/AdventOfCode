module Day6 where

import Data.List.Split


solve :: FilePath -> IO Int
solve fileName =
  do contents <- readFile fileName
     let initState = map readInt $ splitOn "," contents
     return (length $ simulate 256 initState)


simulate :: Int -> [Int] -> [Int]
simulate n xs = sim 0 xs
  where sim i acc | i == n    = acc
                  | otherwise = sim (i+1) (update acc)


update :: [Int] -> [Int]
update lst = update' lst [] []
  where
    update' :: [Int] -> [Int] -> [Int] -> [Int]
    update' [] ys ws     = (reverse ys) ++ (reverse ws)
    update' (x:xs) ys ws =
      let gen_new = x == 0
          new_ws = if gen_new then (8 : ws) else ws
          new_x   = if x == 0 then 6 else x-1
      in update' xs (new_x : ys) new_ws


readInt :: String -> Int
readInt s = read s :: Int
