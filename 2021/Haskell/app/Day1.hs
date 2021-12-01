module Day1 where

-- Takes a file path and an integer representing the sliding window
-- size. To solve Part 1, make n = 1.

solve :: FilePath -> Int -> IO Integer
solve fileName n =
  do xs <- listFromFile fileName
     return (larger xs n)


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


-- -----------------------------------------------------
--                   Helpers

-- Takes a file path an returns a list with each line
-- read as an Integer.

listFromFile :: FilePath -> IO [Integer]
listFromFile fileName =
  do contents <- readFile fileName
     let numbers = map (\x -> read x :: Integer) (lines contents)
     return numbers
