module AOC.Day19 where

import Data.Monoid
import Data.MultiSet as M

import Linear (V3(..), (^+^), (^-^))


solve :: IO (Int,Int)
solve =
  do contents <- readFile "data/day19_input.txt"
     return (1,1)


type Point = V3 Int
type Transform = Endo Point

data Scanner  = Scanner
  { scannerId :: Int
  , beacons   :: [Point]
  , transformation :: Endo Point
  , signature :: M.MultiSet Int
  }
  deriving (Show)


instance Show Transform where
  show c = show $ appEndo c (V3 0 0 0)
