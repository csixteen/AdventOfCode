module Day3 where

import Data.Char (digitToInt)
import Data.List
import Data.Ord (comparing)


solve :: FilePath -> IO Int
solve fileName =
  do contents <- readFile fileName
     let strs = lines contents
     return (power strs)

lifeSupport :: [String] -> Int
lifeSupport xs = (oxygen xs) * (co2 xs)

oxygen :: [String] -> Int
oxygen xs = undefined

co2 :: [String] -> Int
co2 xs = undefined


-- Calculates the power consumption (part 1)
power :: [String] -> Int
power xs = (gamma xs) * (epsilon xs)


-- Calculates the gamma rate (part 1)
gamma :: [String] -> Int
gamma = aggregate mostCommon


-- Calculates the epsilon rate (part 1)
epsilon :: [String] -> Int
epsilon = aggregate leastCommon


aggregate :: (String -> Char) -> [String] -> Int
aggregate f xs =
  let xs' = rate' 0 []
  in convert xs'
  where rate' n acc | n >= length (head xs) = reverse acc
                    | otherwise             = rate' (n+1) (f (colToBinStr n xs) : acc)


{-
  Given an index and a list of binary Strings (all with the
  same length), it builts a new binary String by collecting the
  characters in the specified index from each String in the list.

  e.g.
  ghci> colToBinStr 1 ["0100", "0011", "1001", "1111"]
  "1001"
-}
colToBinStr :: Int -> [String] -> String
colToBinStr n xs = fmap (!! n) xs


-- Finds the most common bit in a binary String.
mostCommon :: String -> Char
mostCommon = head . maximumBy (comparing length ) . group . sort


-- Finds the least common bit in a binary String.
leastCommon :: String -> Char
leastCommon xs = let mc = mostCommon xs
                 in if mc == '1' then '0' else '1'


-- Converts a binary String (a String that only has '0' and '1')
-- to its decimal representation.
convert :: String -> Int
convert = bin2Int . fmap digitToInt . reverse
  where bin2Int [] = 0
        bin2Int (x:xs) = x + 2 * bin2Int xs
