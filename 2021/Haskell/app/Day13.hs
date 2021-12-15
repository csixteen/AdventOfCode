module Day13 where

import Data.List
import Data.List.Split


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       [dots,inst] = splitWhen null $ lines contents
       dots' = fmap (\[a,b] -> (readInt a, readInt b)) $ fmap (splitOn ",") dots
       inst' = instructions inst
       (mRows, mCols) = (
         1 + (maximum $ fmap snd dots'),
         1 + (maximum $ fmap fst dots'))
       g = grid mRows mCols dots'
       oneFold = origami g $ take 1 inst'
       part1 = countDots oneFold
       part2 = origami g inst'
     putStrLn $ intercalate "\n" part2
     return (part1,1)


type Grid  = [[Char]]
type Point = (Int,Int)


origami :: Grid -> [(Char,Int)] -> Grid
origami g inst = foldl' foldGrid g inst


countDots :: Grid -> Int
countDots = length . filter (=='#') . concat


grid :: Int -> Int -> [Point] -> Grid
grid mRows mCols dots = foldl' update g dots
  where
    g = replicate mRows $ replicate mCols '.'


instructions :: [String] -> [(Char,Int)]
instructions is = fmap (\[a,b] -> (a !! 0, readInt b)) is''
  where
    is'  = fmap (last . (splitOn " ")) is
    is'' = fmap (splitOn "=") is'


foldGrid :: Grid -> (Char, Int) -> Grid
foldGrid g ('x',col) = foldByCol g col
foldGrid g ('y',row) = foldByRow g row


foldByCol :: Grid -> Int -> Grid
foldByCol g col = combine left right
  where
    left  = map (reverse . take col) g
    right = map (drop (col+1)) g


foldByRow :: Grid -> Int -> Grid
foldByRow g row = combine top bottom
  where
    top    = reverse $ take row g
    bottom = drop (row+1) g


combine :: Grid -> Grid -> Grid
combine (x:xs) (y:ys) = (combine' x y) : combine xs ys
combine xs []         = xs
combine [] ys         = ys


combine' :: [Char] -> [Char] -> [Char]
combine' (x:xs) (y:ys) = c : combine' xs ys
  where
    c = if x == '#' || y == '#' then '#' else '.'
combine' xs [] = xs
combine' [] ys = ys


update :: Grid -> Point -> Grid
update (r:rs) (col,0)   = (updateRow r col) : rs
update (r:rs) (col,row) = r : update rs (col,row-1)
update [] _             = error "Row out of bounds"


updateRow :: [Char] -> Int -> [Char]
updateRow (_:cs) 0   = '#' : cs
updateRow (c:cs) col = c : updateRow cs (col-1)
updateRow [] _       = error "Column out of bounds"


readInt :: String -> Int
readInt s = read s
