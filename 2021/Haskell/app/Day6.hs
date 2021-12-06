module Day6 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as S


solve :: FilePath -> IO (Int, Int)
solve fileName =
  do contents <- readFile fileName
     let lst   = map readInt $ splitOn "," contents
         s     = state lst
         part1 = sum $ S.elems $ (iterate update s) !! 80
         part2 = sum $ S.elems $ (iterate update s) !! 256
     return (part1, part2)


type State = S.Map Int Int


state :: [Int] -> State
state xs = foldl' f s0 xs
  where
    s0 = S.fromList $ zip [0..8] (replicate 9 0)
    f :: State -> Int -> State
    f m i = S.insertWith (+) i 1 m


update :: State -> State
update s =
  let
    (x : xs) = S.elems s
    s' = S.fromList $ zip [0..8] xs
    s'' = S.insert 8 x s'
  in S.adjust (+ x) 6 s''


readInt :: String -> Int
readInt s = read s :: Int
