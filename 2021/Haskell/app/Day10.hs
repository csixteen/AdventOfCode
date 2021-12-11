module Day10 where

import Data.Maybe


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       part1  = sum $ map (fromJust . fst) $ filter (isJust . fst) $ map validate $ lines contents
     return (part1,1)


matches :: [(Char,Char)]
matches = [
  ('(', ')'),
  ('[', ']'),
  ('{', '}'),
  ('<', '>')]


findMatch :: Char -> Char
findMatch c =
  case lookup c matches of
    Just x  -> x
    Nothing -> fst $ head $ filter ((==c) . snd) matches


points :: Char -> Int
points c = fromJust $ lookup c pts
  where
    pts = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]


validate :: String -> (Maybe Int, String)
validate chunks =
  case validate' chunks [] of
    Nothing -> (Nothing, chunks)
    Just c  -> (Just (points c), chunks)


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
    

-- Given a char, returns a Maybe Char with the matching
-- opening bracket.
isOpening :: Char -> Bool
isOpening c = isJust $ lookup c matches


isClosing :: Char -> Bool
isClosing = not . isOpening
