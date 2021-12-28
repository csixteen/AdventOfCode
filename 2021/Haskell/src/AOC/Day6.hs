module AOC.Day6 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as S


solve :: FilePath -> IO (Int, Int)
solve fileName =
  do contents <- readFile fileName
     let lst   = map readInt $ splitOn "," contents
         s     = state lst
         part1 = sum $ S.elems $ (iterate update s) !! 80
         part2 = sum $ S.elems $ (iterate update s) !! 256
     return (part1, part2)


-- The State is a mapping between days until end of cycle and the
-- number of lanternfish in such state.
type State = S.Map Int Int


-- Takes a list of integers and builds the initial state.
state :: [Int] -> State
state xs = foldl' f s0 xs
  where
    s0 = S.fromList $ zip [0..8] (repeat 0)
    f :: State -> Int -> State
    f m i = S.insertWith (+) i 1 m


-- This function takes a State and returns a new State
-- with the mappings updated.
update :: State -> State
update s =
  let
    -- S.elems returns all the values sorted according to the
    -- ascending order of the keys.
    (x : xs) = S.elems s
    -- Builds a new State where the key 8 is set to 0. All other
    -- keys are deducted from a shift in the values.
    s' = S.fromList $ zip [0..8] xs
    -- Updates the key 8 with the previous value of the key 0
    s'' = S.insert 8 x s'
  -- Updates the key 6 by adding the number of lanterfish that
  -- had 0 days remaining to the end of the cycle.
  in S.adjust (+ x) 6 s''


readInt :: String -> Int
readInt s = read s :: Int
