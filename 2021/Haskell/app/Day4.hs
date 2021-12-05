module Day4 where

import Data.List
import Data.List.Split
import Data.Maybe

type Cell = (Int, Bool)
type Board = [[Cell]]


solve :: FilePath -> IO (Int, Int)
solve fileName =
  do contents <- readFile fileName
     let strs     = filter ((> 0) . length) $ lines contents
         numbers  = fmap readInt $ splitOn "," (head strs)
         boards   = bingoFromList $ tail strs
         (n1, b1) = play numbers boards
         (n2, b2) = lastBoard numbers boards
     return (n1 * sumBoard b1, n2 * sumBoard b2)


play :: [Int] -> [Board] -> (Int, Board)
play (n:ns) bs =
  let bs' = map (update n) bs
  in case existsWinner bs' of
    Nothing -> play ns bs'
    Just b  -> (n, b)


lastBoard :: [Int] -> [Board] -> (Int, Board)
lastBoard ns bs = (n, head winners)
  where
    bs' :: [([Board], [Board], Int)]
    bs' = history ns bs
    (_, winners, n) = fromJust $ find (\(losers, winners', n') -> null losers) bs'


history :: [Int] -> [Board] -> [([Board], [Board], Int)]
history ns bs = scanl part (bs, [], -1) ns
  where
    part :: ([Board], [Board], Int) -> Int -> ([Board], [Board], Int)
    part (bs, _, _) n = (losers, winners, n)
      where (winners, losers) = partition winner $ map (update n) bs


update :: Int -> Board -> Board
update n b =
  case index n b of
    Nothing         -> b
    Just (row, col) -> mark row col b


existsWinner :: [Board] -> Maybe Board
existsWinner []     = Nothing
existsWinner (b:bs) = case winner b of
                        True  -> Just b
                        False -> existsWinner bs


winner :: Board -> Bool
winner b = winner' b || winner' (transpose b)
  where winner' = any winRow
        winRow  = all snd


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
