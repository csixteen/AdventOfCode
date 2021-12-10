module Day9 where

import Data.Char
import Data.List
import qualified Data.Set as S


solve :: FilePath -> IO (Int, Int)
solve fileName =
  do contents <- readFile fileName
     let
       hm    = heatmap $ lines contents
       part1 = sum $ map ((+1) . (point hm)) $ lowPoints hm
       part2 = product $ take 3 $ sortBy (flip compare) $ map length $ basins hm
     return (part1,part2)


type Heatmap = [[Int]]
type Basin   = [(Int,Int)]
type PairSet = S.Set (Int,Int)


basins :: Heatmap -> [Basin]
basins hm = map (basin hm) $ lowPoints hm


basin :: Heatmap -> (Int,Int) -> Basin
basin hm p = S.toList acc
  where
    (acc, _) = basin' hm p S.empty S.empty


basin' :: Heatmap -> (Int,Int) -> PairSet -> PairSet -> (PairSet, PairSet)
basin' hm p visited acc
  | (S.member p visited) || (9 == point hm p) = (acc, visited)
  | otherwise = foldr addNeighbor (S.insert p acc, S.insert p visited) $ neighbors p hm
  where
    addNeighbor :: (Int,Int) -> (PairSet, PairSet) -> (PairSet, PairSet)
    addNeighbor neighbor (acc', visited') =
      let (acc'', visited'') = basin' hm neighbor visited' acc'
      in  (S.union acc' acc'', S.union visited' visited'')


lowPoints :: Heatmap -> [(Int,Int)]
lowPoints hm = filter (isLowPoint hm) points
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
