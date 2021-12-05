module Day4 where

import Data.List
import Data.List.Split

type Cell = (Int, Bool)
type Board = [[Cell]]


solve :: FilePath -> IO Int
solve fileName =
  do contents <- readFile fileName
     let strs       = filter ((> 0) . length) $ lines contents
         numbers    = fmap readInt $ splitOn "," (head strs)
         boards     = bingoFromList $ tail strs
         (n, board) = play numbers boards
     return (n * sumBoard board)


play :: [Int] -> [Board] -> (Int, Board)
play (n:ns) bs =
  let bs' = map (update n) bs
  in case existsWinner bs' of
    Nothing -> play ns bs'
    Just b  -> (n, b)


update :: Int -> Board -> Board
update n b =
  case index n b of
    Nothing         -> b
    Just (row, col) -> mark row col b


existsWinner :: [Board] -> Maybe Board
existsWinner []     = Nothing
existsWinner (b:bs) =
  case winner b of
    False -> existsWinner bs
    True  -> Just b


winner :: Board -> Bool
winner b = winner' b || winner' (transpose b)
  where winner' = foldl' (||) False . fmap (all snd)


sumBoard :: Board -> Int
sumBoard = sum . map (foldl' (\acc x -> acc + fst x) 0 . filter (not . snd))


index :: Int -> Board -> Maybe (Int, Int)
index n b = findInBoard 0 b
  where findInBoard _ [] = Nothing
        findInBoard row (b':bs') =
          case findPos n b' of
            Nothing  -> findInBoard (row+1) bs'
            Just col -> Just (row, col)


findPos :: Int -> [Cell] -> Maybe Int
findPos n row =
  let indices = map fst $ filter ((n==) . fst . snd) $ zip [0..] row
  in case indices of
    [] -> Nothing
    xs -> Just (head xs)


mark :: Int -> Int -> Board -> Board
mark r c b = (take r b) ++ ((mark' c $ b !! r) : (drop (r+1) b))
  where mark' col row = (take col row) ++ ((fst $ row !! col, True) : (drop (col+1) row))


bingoFromList :: [String] -> [Board]
bingoFromList [] = []
bingoFromList xs = rawToBoard (take 5 xs) : bingoFromList (drop 5 xs)


rawToBoard :: [String] -> Board
rawToBoard []     = []
rawToBoard (x:xs) =
  let row = fmap (\n -> (readInt n, False)) $ words x
  in row : rawToBoard xs


readInt :: String -> Int
readInt i = read i :: Int
