module Day8 where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as S
import Data.Maybe


solve :: FilePath -> IO (Int, Int)
solve fileName =
  do contents <- readFile fileName
     let raw1  = fmap (splitOn " | ") $ lines contents
         raw2  = fmap (\row -> fmap words row) raw1
         part1 = occurrences (fmap (!! 1) raw2) [1, 4, 7, 8]
         part2 = sum $ fmap (\[ps,o] -> outVal ps o) raw2
     return (part1,part2)


type Patterns = S.Map String [Int]


{-
 Maps each digit to the segments that are turned on:

    0
  ----
1|    |2
 |  3 |
  ----
4|    |5
 |    |
  ----
    6

-}
type Segments = S.Map Int [Int]


segments :: Segments
segments = S.fromList $ [
  (0, [0, 1, 2, 4, 5, 6])
 ,(1, [2, 5])
 ,(2, [0, 2, 3, 4, 6])
 ,(3, [0, 2, 3, 5, 6])
 ,(4, [1, 2, 3, 5])
 ,(5, [0, 1, 3, 5, 6])
 ,(6, [0, 1, 3, 4, 5, 6])
 ,(7, [0, 2, 5])
 ,(8, [0, 1, 2, 3, 4, 5, 6])
 ,(9, [0, 1, 2, 3, 5, 6])]

-- Takes a list of patterns and a list of output notes and returns
-- the integer that results from the concatenation of the digits
-- associated with each output note.
outVal :: [String] -> [String] -> Int
outVal ps os = read digits
  where
    digits = map intToDigit $ fmap (\o -> fromJust $ lookup (sort o) guesses) os
    guesses :: [(String, Int)]
    guesses = guess $ patterns ps


-- Given a list of patterns, builds an initial mapping between
-- patterns and digits.
patterns :: [String] -> Patterns
patterns ps = foldl' addPattern S.empty ps
  where
    addPattern m p = S.insert p (candidates p) m


-- Given a string pattern, returns all the possible digit candidates
-- for such pattern.
candidates :: String -> [Int]
candidates s = map fst $ filter ((==l) . length . snd) $ S.assocs segments
  where l = length s


guess :: Patterns -> [(String, Int)]
guess pats =
  if all isSingleton $ S.elems  pats
  then fmap (\(k, v) -> (sort k, head v)) $ S.assocs pats
  else
    let
      (certain, uncertain) = S.partition isSingleton pats
      (p, x:xs) = head $ sortBy (\(_, a) (_, b) -> compare (length a) (length b)) $ S.assocs uncertain
    in
      case validCandidate x p certain of
        True  -> guess $ S.insert p [x] pats
        False -> guess $ S.insert p xs pats


isSingleton :: [a] -> Bool
isSingleton = (==1) . length


-- Given a mapping between patterns and digits, it returns a list of the
-- digits whose association we're certain of. In other words, it returns
-- the digits that belong to one-element lists associated to patterns.
matched :: Patterns -> [Int]
matched pats = map (head . snd) $ filter (isSingleton . snd) $ S.assocs pats


-- Given a digit, a pattern and a mapping between patterns and digits, it
-- tells whether the digit is a valid candidate for the given pattern.
validCandidate :: Int -> String -> Patterns -> Bool
validCandidate c p pats = all (\o -> (lookup o inter) == (lookup o xs)) others
  where
    others = matched pats
    inter  = filter (\(i,_) -> i `elem` others) $ intersections c
    xs     = fmap (\o -> (o, length $ intersect p $ digitToPattern o pats)) others


-- Given a source digit, it returns a list with the number of segments that
-- the source digit intersects with all the other digits.
intersections :: Int -> [(Int, Int)]
intersections n = fmap (\(k, v) -> (k, length $ intersect segs v)) other
  where
    segs  = segments S.! n
    other = S.assocs $ S.filterWithKey (\k _ -> k /= n) segments


-- Given a digit and a mappign between patterns and candidates,
-- returns the pattern associated to such digit.
digitToPattern :: Int -> Patterns -> String
digitToPattern d ps = fst $ head $ filter ((d `elem`) . snd) $ S.assocs ps


occurrences :: [[String]] -> [Int] -> Int
occurrences output ns = sum $ map (occur $ concat output) ns
  where
    occur :: [String] -> Int -> Int
    occur entries n = length $ filter (\x -> n `elem` (fromLength (length x))) entries


fromLength :: Int -> [Int]
fromLength l = map fst $ filter ((==l) . length . snd) $ S.assocs segments
