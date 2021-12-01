module Day1 where


import System.IO
import Text.Read


solve :: FilePath -> Int -> IO Integer
solve fileName n =
  do xs <- listFromFile fileName
     case xs of
       Nothing  -> error "Bad input"
       Just xs' -> return (larger xs' n)


-- Takes a list of Integers and the size of a sliding winndow and
-- returns the number of sliding windows whose sum is greater than
-- the sum of the previous window.

larger :: [Integer] -> Int -> Integer
larger xs n | length xs < n = error "Bad list!"
            | otherwise     = l_than (tail xs) (sum $ take n xs) 0
  where
    l_than xs' prev acc | length xs' < n = acc
                        | otherwise      = let new_prev = sum $ take n xs'
                                               new_acc  = if new_prev > prev then acc + 1 else acc
                                           in l_than (tail xs') new_prev new_acc


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
