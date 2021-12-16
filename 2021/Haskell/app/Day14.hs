module Day14 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       [template, rs] = splitWhen null $ lines contents
       rs'            = rules rs
       (part1,_)      = (iterate polymerization (template !! 0, rs')) !! 10
     putStrLn $ show $ calculate part1
     return (1,1)


type Rules = M.Map [Char] Char
type State = (String,Rules)


calculate :: String -> Int
calculate xs = (last count) - (head count)
  where
    count     = sort $ fmap snd $ M.toList $ foldl' ins M.empty xs
    ins acc c = M.insertWith (+) c 1 acc


polymerization :: State -> State
polymerization (tplt, rs) = (tplt', rs)
  where
    tplt'    = intercalate' tplt mappings
    mappings = fmap (rs M.!) $ zipWith (\a b -> [a,b]) tplt (tail tplt)


rules :: [String] -> Rules
rules xs = foldl' addRule M.empty xs''
  where
    xs'  = fmap (splitOn " -> ") xs
    xs'' = fmap (\[a,b] -> (a,b !! 0)) xs'
    addRule rs (a,b) = M.insert a b rs 


intercalate' :: [Char] -> [Char] -> String
intercalate' (x:xs) (y:ys) = x : y : intercalate' xs ys
intercalate' xs []         = xs
intercalate' [] ys         = ys
