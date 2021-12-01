module Day1 where


import System.IO
import Text.Read


solve :: FilePath -> ([Integer] -> Integer) -> IO Integer
solve fileName f =
  do xs <- listFromFile fileName
     case xs of
       Nothing  -> error "Bad input"
       Just xs' -> return (f xs')


-- Solves part 1, by simply counting the number of entries in the list
-- that are larger than the previous one.
larger_than :: [Integer] -> Integer
larger_than [] = error "Empty list!"
larger_than [_] = 0
larger_than xs = l_than xs (head xs) 0
  where
    l_than [] _ acc = acc
    l_than (y:ys) prev acc =
          let new_acc = if y > prev then acc + 1 else acc
          in l_than ys y new_acc


-- Solves part 2, by counting the number of sliding windows of length 3 that
-- are larger than the previous sliding window. This is very generic (it wouldn't
-- work for a N-size sliding window.
larger_than_sliding :: [Integer] -> Integer
larger_than_sliding xs@(_:_:_:_) = l_than (drop 1 xs) (sum $ (take 3 xs)) 0
  where
    l_than xs' prev acc | l < 3 = acc
                        | otherwise = let new_prev = sum $ take 3 xs'
                                          new_acc  = if new_prev > prev then acc + 1 else acc
                                      in l_than (drop 1 xs') new_prev new_acc
                        where l = length xs'
larger_than_sliding _ = error "Bad list!"


-- -----------------------------------------------------------

-- Helpers for reading input from a file as a list of Integers

entryFromHandle :: Handle -> IO (Maybe Integer)
entryFromHandle fh =
  do n <- hGetLine fh
     case readMaybe n :: Maybe Integer of
       Nothing -> return Nothing
       Just x  -> return (Just x)


listFromHandle :: Handle -> IO (Maybe [Integer])
listFromHandle fh =
  do ended <- hIsEOF fh
     if ended then return (Just [])
     else
       do x <- entryFromHandle fh
          case x of
            Nothing -> return Nothing
            Just x' ->
              do xs <- listFromHandle fh
                 case xs of
                   Nothing  -> return Nothing
                   Just xs' -> return (Just (x' : xs'))


listFromFile :: FilePath -> IO (Maybe [Integer])
listFromFile fileName =
  do fh <- openFile fileName ReadMode
     xs <- listFromHandle fh
     hClose fh
     return xs
