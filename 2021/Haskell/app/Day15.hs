module Day15 where

import Data.Char
import Data.List
import qualified Data.Map.Strict as M


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       gs = grid $ lines contents
       part1 = dp gs
     return (part1,1)


type Grid  = [[Int]]
type Point = (Int,Int)
type Cache = M.Map Point Int


grid :: [String] -> Grid
grid = fmap (fmap digitToInt)


val :: Grid -> Point -> Int
val gs (r,c) = (gs !! r) !! c


bounds :: Grid -> (Int,Int)
bounds gs = ((length gs)-1, (length $ head gs)-1)


inBounds :: Grid -> Point -> Bool
inBounds gs (r,c) = (r `elem` [0..mRows]) && (c `elem` [0..mCols])
  where (mRows, mCols) = bounds gs


dp :: Grid -> Int
dp gs = cost - val gs (0,0)
  where
    (cost, _) = dp' gs (0,0) M.empty


dp' :: Grid -> Point -> Cache -> (Int, Cache)
dp' gs (r,c) cache
  | (r,c) == bounds gs =
    let v = val gs (r,c)
    in (v, M.insert (r,c) v cache)
  | M.member (r,c) cache = (cache M.! (r,c), cache)
  | otherwise = (i', M.insert (r,c) i' cs) 
  where
    i' = i + val gs (r,c)
    (i,cs) = foldl' foobar (1000000000, cache) points
    foobar (v,cache') pt =
      let (v',cache'') = dp' gs pt cache'
      in (min v v', M.union cache' cache'')
    points = filter (inBounds gs) [(r+1,c), (r,c+1)]
