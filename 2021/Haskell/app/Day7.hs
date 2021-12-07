module Day7 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as S


solve :: FilePath -> IO (Int, Int)
solve fileName =
  do contents <- readFile fileName
     let
       ns = map readInt $ splitOn "," contents
       part1 = allCosts ns costs
       part2 = allCosts ns costs2
     return (minimum part1, minimum part2)


type CostFn = [Int] -> Int -> Int


-- Takes a list of origin positions and a target position and returns
-- the total fuel cost from all the origins to that target (part 1).
costs :: [Int] -> Int -> Int
costs nums pos = sum $ fmap (\n -> abs (n-pos)) nums


-- Fuel cost for part 2.
costs2 :: [Int] -> Int -> Int
costs2 nums pos = sum $ fmap (cost pos) nums
  where
    cost pos' n =
      let x = abs (pos' - n)
      in x * (x + 1) `div` 2


-- Takes a list of positions and returns a list with all the fuel costs
-- to all the positions between 0 and the maximum position, using the
-- cost function CostFn.
allCosts :: [Int] -> CostFn -> [Int]
allCosts ns f = fmap (f ns) [0..(maximum ns)]


readInt :: String -> Int
readInt s = read s
