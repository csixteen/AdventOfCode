module AOC.Day20 where

import Data.Ix
import Data.List
import Data.List.Split
import qualified Data.Set as S

import Linear (V2(..), (^-^), (^+^))


solve :: IO (Int, Int)
solve =
  do contents <- readFile "data/day20_input.txt"
     let
       [enh, img] = splitWhen null $ lines contents
       enh'       = (==('#')) <$> (head enh)
       img'       = mkImage img
       part1      = S.size $ grid $ snd $ (iterate step (enh', img')) !! 2
       part2      = S.size $ grid $ snd $ (iterate step (enh', img')) !! 50
     return (part1,part2)


-- ------------------------------------------
--       Type declarations and helpers
-- ------------------------------------------


type Point  = V2 Int
type Grid   = S.Set Point
type Region = (Point,Point)

data Image = Image
  { grid    :: Grid
  , region  :: Region
  , distant :: Bool
  }
  deriving (Show)

type Enhancement = [Bool]
type State = (Enhancement, Image)

mkImage :: [String] -> Image
mkImage xs = Image { grid    = px
                   , region  = (V2 0 0, V2 x y)
                   , distant = False
                   }
  where
    x = (length $ head xs) - 1
    y = length xs - 1
    px = S.fromList [V2 x' y' | y' <- [0..y], x' <- [0..x], (xs !! y') !! x' == '#']


-- -----------------------------
--      Solvers and helpers
-- -----------------------------


step :: State -> State
step (enh, im@Image{..}) = (enh, Image { grid = grid'
                                       , region = region'
                                       , distant = if distant then (last enh) else (head enh)
                                    })
  where
    grid'   = foldl' (addPixel enh im) S.empty pts
    pts     = range region'
    region' = expand region


addPixel :: Enhancement -> Image -> Grid -> Point -> Grid
addPixel enh img acc pt = if enh !! i then S.insert pt acc else acc
  where
    i = binToInt $ (pixel img) <$> neighbors pt


pixel :: Image -> Point -> Bool
pixel Image{..} pt | inRange region pt = S.member pt grid
                   | otherwise         = distant


expand :: Region -> Region
expand (a, b) = (a ^-^ V2 1 1, b ^+^ V2 1 1)


neighbors :: Point -> [Point]
neighbors p = [p ^+^ (V2 x y) | y <- [-1, 0, 1], x <- [-1, 0, 1]]


binToInt :: [Bool] -> Int
binToInt bs = foldl' asB 0 bs
  where
    asB acc d = (2 * acc) + (if d then 1 else 0)
