module Day11 where

import Data.Char
import Data.List
import qualified Data.Set as S


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       m     = matrix $ lines contents
       part1 = (iterate step (0,m)) !! 100
       part2 = synchronized (0,m) 0
     return (fst part1,part2)


type Point  = (Int,Int)
type Matrix = [[Int]]
type State  = (Int, Matrix)


matrix :: [String] -> Matrix
matrix = map (\row -> map digitToInt row)


bounds :: Matrix -> (Int, Int)
bounds m = ((length m) - 1, (length $ head m) - 1)


points :: Matrix -> [Point]
points m = [(x,y) | x <- [0..mRows], y <- [0..mCols]]
  where
    (mRows, mCols) = bounds m


val :: Matrix -> Point -> Int
val m (x,y) = (m !! x) !! y


synchronized :: State -> Int -> Int
synchronized (s,m) i | (sum $ map sum m) == 0 = i
                     | otherwise = synchronized (step (s,m)) (i+1)


step :: State -> State
step (z, m) = (z + z', m'')
  where
    z' = length $ S.toList flsh
    m' = foldl' (\acc pt -> update acc (+1) pt) m $ points m
    (flsh, m'') = increment m' S.empty


increment :: Matrix -> S.Set Point -> (S.Set Point, Matrix)
increment m acc =
  case filter (\p -> not $ S.member p acc) $ flashing m of
    [] -> (acc, foldl' (\a pt -> zero a pt) m $ points m)
    xs -> increment m' acc'
      where
        v    = foldl' (\a pt -> foldr (:) a $ neighbors pt m) [] xs
        m'   = foldl' (\a pt -> update a (+1) pt) m v
        acc' = foldl' (\a pt -> S.insert pt a) acc xs


zero :: Matrix -> Point -> Matrix
zero m p | (val m p) > 9 = update m (\_ -> 0) p
         | otherwise     = m


flashing :: Matrix -> [Point]
flashing m = filter ((> 9) . (val m)) $ points m


update :: Matrix -> (Int -> Int) -> Point -> Matrix
update (r:rs) f (0, col)   = (updateRow r col f) : rs
update (r:rs) f (row, col) = r : update rs f (row-1, col)
update [] _ _              = error "Row out of bounds"


updateRow :: [Int] -> Int -> (Int -> Int) -> [Int]
updateRow (c:cs) 0 f   = (f c) : cs
updateRow (c:cs) col f = c : (updateRow cs (col-1) f)
updateRow [] _ _       = error "Column out of bounds"


neighbors :: Point -> Matrix -> [Point]
neighbors p m = filter validPoint $ neighbors' p
  where
    validPoint (x,y) = (between x 0 mRows) && (between y 0 mCols)
    (mRows, mCols)   = bounds m
    between a b c    = a `elem` [b..c]


neighbors' :: Point -> [Point]
neighbors' p = zipWith sumPairs increments (repeat p)
  where
    sumPairs (a,b) (c,d) = (a+c, b+d)
    increments = [(x,y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]
