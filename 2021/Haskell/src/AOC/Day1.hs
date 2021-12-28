module AOC.Day1 where

-- Takes a file path and an integer representing the sliding window
-- size. To solve Part 1, make n = 1.

solve :: FilePath -> Int -> IO Int
solve fileName n =
  do xs <- listFromFile fileName
     let res = negatives $ diff_list $ sum_list xs n
     return res

negatives :: [Int] -> Int
negatives xs = length $ filter (< 0) xs

diff_list :: [Int] -> [Int]
diff_list xs = fmap (uncurry (-)) $ zip xs (tail xs)

sum_list :: [Int] -> Int -> [Int]
sum_list xs n | length xs < n = []
              | otherwise     = (sum (take n xs)) : sum_list (tail xs) n

-- -----------------------------------------------------
--                   Helpers

-- Takes a file path an returns a list with each line
-- read as an Integer.

listFromFile :: FilePath -> IO [Int]
listFromFile fileName =
  do contents <- readFile fileName
     let numbers = fmap (\x -> read x :: Int) $ lines contents
     return numbers
