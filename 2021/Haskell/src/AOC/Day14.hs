module AOC.Day14 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       [ts, rs]  = splitWhen null $ lines contents
       templ     = template $ ts !! 0
       rs'       = rules rs
       (part1,_) = (iterate polymerization (templ, rs')) !! 10
       (part2,_) = (iterate polymerization (templ, rs')) !! 40
     return (calculate part1, calculate part2)


type Rules    = M.Map [Char] Char
type Template = M.Map [Char] Int
type State    = (Template,Rules)


calculate :: Template -> Int
calculate ts = (x - y) `div` 2
  where
    x      = last count'
    y      = head count'
    count' = sort $ fmap snd $ M.toList count
    count  = foldl' addCount M.empty $ M.toList ts
    addCount :: M.Map Char Int -> ([Char], Int) -> M.Map Char Int
    addCount acc ([a,b],cnt) = acc''
      where
        acc'  = M.insertWith (+) a cnt acc
        acc'' = M.insertWith (+) b cnt acc'


polymerization :: State -> State
polymerization (tplt, rs) = (tplt', rs)
  where
    tplt' = foldl' (addPair rs) M.empty $ M.toList tplt


addPair :: Rules -> Template -> ([Char], Int) -> Template
addPair rs ts ([a,b],cnt) = ts''
  where
    x    = rs M.! [a,b]
    ts'  = M.insertWith (+) [a,x] cnt ts
    ts'' = M.insertWith (+) [x,b] cnt ts'


rules :: [String] -> Rules
rules xs = foldl' addRule M.empty xs''
  where
    xs'  = fmap (splitOn " -> ") xs
    xs'' = fmap (\[a,b] -> (a,b !! 0)) xs'
    addRule rs (a,b) = M.insert a b rs


template :: String -> Template
template ts = foldl' ins M.empty $ zip ts (tail ts)
  where
    ins acc (a,b) = M.insertWith (+) [a,b] 1 acc
