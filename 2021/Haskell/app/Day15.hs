module Day15 where

import Data.Char
import Data.List


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       rs = risks $ lines contents
       part1 = navigate rs
     return (part1,1)


navigate :: [[Int]] -> Int
navigate rs = last $ navigate' (start $ head rs) (tail rs)


navigate' :: [Int] -> [[Int]] -> [Int]
navigate' prev (r:rs) = navigate' (update prev r) rs
navigate' prev []     = prev


risks :: [String] -> [[Int]]
risks = fmap (fmap digitToInt)


start :: [Int] -> [Int]
start xs = scanl (+) 0 $ tail xs


update :: [Int] -> [Int] -> [Int]
update prev rs = reverse $ foldl' update' [x] [1..l]
  where
    x = (rs !! 0) + (prev !! 0)
    l = (length rs) - 1
    update' acc i =
      let
        p1 = acc !! 0
        p2 = prev !! i
        r  = rs !! i
      in
        (r + (min p1 p2)) : acc
