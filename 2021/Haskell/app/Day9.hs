module Day9 where

import Data.Char
import Debug.Trace


solve :: FilePath -> IO (Int, Int)
solve fileName =
  do contents <- readFile fileName
     let
       hm    = heatmap $ lines contents
       part1 = sum $ lowPoints hm
     return (part1,1)


type Heatmap = [[Int]]


lowPoints :: Heatmap -> [Int]
lowPoints hm = map ((+1) . (point hm)) $ filter (isLowPoint hm) points
  where
    points         = [(x,y) | x <- [0..mRows], y <- [0..mCols]]
    (mRows, mCols) = ((length hm)-1, (length $ head hm)-1)


point :: Heatmap -> (Int,Int) -> Int
point hm (row,col) = (hm !! row) !! col


isLowPoint :: Heatmap -> (Int,Int) -> Bool
isLowPoint hm p = all (\n -> (point hm p) < (point hm n)) neighbs
  where
    neighbs = neighbors p hm


heatmap :: [String] -> Heatmap
heatmap = map (\s -> map digitToInt s)


neighbors :: (Int, Int) -> Heatmap -> [(Int, Int)]
neighbors p hm = filter (\(x,y) -> (between x 0 mRows) && (between y 0 mCols)) candidates
  where
    candidates           = zipWith sumPairs (repeat p) increments
    between x a b        = x `elem` [a..b]
    increments           = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    (mRows, mCols)       = ((length hm)-1, (length $ head hm)-1)
    sumPairs (a,b) (c,d) = (a+c, b+d)


readInt :: String -> Int
readInt s = read s
