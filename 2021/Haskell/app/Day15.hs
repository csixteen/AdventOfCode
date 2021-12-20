module Day15 where

import Data.Char
import Data.List


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       rs    = risks $ lines contents
       part1 = navigate rs
       part2 = navigate $ extend rs
     return (part1,part2)


navigate :: [[Int]] -> Int
navigate rs = last $ navigate' (start $ head rs) (tail rs)


navigate' :: [Int] -> [[Int]] -> [Int]
navigate' prev rs = foldl' (\acc r -> update acc r) prev rs


extend :: [[Int]] -> [[Int]]
extend rs =
  concat $
  fmap (\(i,rs') -> fmap (extendRow i) rs') $
  zip [0..4] (repeat rs)


extendRow :: Int -> [Int] -> [Int]
extendRow j row =
  concat $
  fmap (\(i,xs) -> map (\c -> 1+((c+i-1) `mod` 9)) xs) $
  zip [(0+j)..(4+j)] (repeat row)


risks :: [String] -> [[Int]]
risks = fmap (fmap digitToInt)


start :: [Int] -> [Int]
start = scanl (+) 0 . tail


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
