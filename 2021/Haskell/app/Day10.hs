module Day10 where

import Data.List
import Data.Maybe


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       part1  = sum $ map (fromJust . fst) $ filter (isJust . fst) $ map validate $ lines contents
       scores = map (complete . snd) $ filter (isNothing . fst) $ map validate $ lines contents
       part2  = (sort scores) !! ((length scores) `div` 2)
     return (part1,part2)


matches :: [(Char,Char)]
matches = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]


findMatch :: Char -> Char
findMatch c =
  case lookup c matches of
    Just x  -> x
    Nothing -> fst $ head $ filter ((==c) . snd) matches


points :: Char -> Maybe Int
points c = lookup c pts
  where
    pts = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]


points2 :: Char -> Maybe Int
points2 c = lookup c pts
  where
    pts = [(')', 1), (']', 2), ('}', 3), ('>', 4)]


complete :: String -> Int
complete chunks = foldl' score 0 res
  where
    score acc c = acc*5 + (fromJust $ points2 c)
    res = complete' chunks []


complete' :: String -> [Char] -> [Char]
complete' [] stack = map findMatch stack
complete' (x:xs) s =
  case isOpening x of
    True  -> complete' xs (x:s)
    False -> complete' xs (tail s)


validate :: String -> (Maybe Int, String)
validate chunks =
  case validate' chunks [] of
    Nothing -> (Nothing, chunks)
    Just c  -> (points c, chunks)


validate' :: String -> [Char] -> Maybe Char
validate' [] _     = Nothing
validate' (x:xs) s =
  case isOpening x of
    True  -> validate' xs (x:s)
    False ->
      case s of
        []   -> error "Bad input"
        y:ys -> if (findMatch x) == y then validate' xs ys
                else Just x
    

isOpening :: Char -> Bool
isOpening c = isJust $ lookup c matches


isClosing :: Char -> Bool
isClosing = not . isOpening
