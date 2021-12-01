module Day1 where


import System.IO
import Text.Read


solve1 :: FilePath -> IO Integer
solve1 fileName =
  do xs <- listFromFile fileName
     case xs of
       Nothing  -> error "Bad input"
       Just xs' -> return (larger_than xs')


larger_than :: [Integer] -> Integer
larger_than [] = error "Empty list!"
larger_than [_] = 0
larger_than xs = l_than xs (head xs) 0
  where
    l_than [] _ acc = acc
    l_than (y:ys) prev acc =
          let new_acc = if y > prev then acc + 1 else acc
          in l_than ys y new_acc


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
