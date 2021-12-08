module Day8 where

import Data.List.Split
import qualified Data.Map.Strict as S


solve :: FilePath -> IO (Int, Int)
solve fileName =
  do contents <- readFile fileName
     let raw1  = fmap (splitOn " | ") $ lines contents
         raw2  = fmap (\row -> fmap words row) raw1
         part1 = occurrences (fmap (!! 1) raw2) [1, 4, 7, 8]
     return (part1,1)

type Digits = S.Map Int Int


digits :: Digits
digits = S.fromList $ [
  (0, 6)
 ,(1, 2)
 ,(2, 5)
 ,(3, 5)
 ,(4, 4)
 ,(5, 5)
 ,(6, 6)
 ,(7, 3)
 ,(8, 7)
 ,(9, 6)]


occurrences :: [[String]] -> [Int] -> Int
occurrences output ns = sum $ map (occur $ concat output) ns
  where
    occur :: [String] -> Int -> Int
    occur entries n = length $ filter (\x -> n `elem` (fromLength (length x))) entries


fromLength :: Int -> [Int]
fromLength l = map fst $ filter ((==l) . snd) $ S.assocs digits
