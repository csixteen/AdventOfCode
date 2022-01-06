module AOC.Day20 where

import Data.Char
import Data.Ix
import Data.List
import Data.List.Split
import qualified Data.Set as S

import Linear (V2(..), (^-^), (^+^))


solve :: IO (Int, Int)
solve =
  do contents <- readFile "data/day20_input.txt"
     let
       [alg, img] = splitWhen null $ lines contents
       enh        = (==('#')) <$> (head alg)
       img'       = mkImage img
     return (1,1)


-- ------------------------------------------
--       Type declarations and helpers
-- ------------------------------------------

type Point = V2 Int
type Grid  = S.Set Point

data Image = Image
  { pixels :: Grid
  , region :: (Point, Point)
  }


mkImage :: [String] -> Image
mkImage xs = Image { pixels = px
                   , region = (V2 0 0, V2 x y)
                   }
  where
    x = (length $ head xs) -1
    y = length xs -1
    px = S.fromList [V2 x' y' | x' <- [0..x], y' <- [0..y], (xs !! y') !! x' == '#']


type Enhancement = [Bool]
type State = (Enhancement, Image)


-- -----------------------------
--      Solvers and helpers
-- -----------------------------


step :: State -> State
step (enh, Image{..}) = (enh, Image { pixels = pixels'
                                    , region = region'
                                    })
  where
    region' = expand region
    pixels' = foldl' addPixel S.empty pts
    (_, V2 x y) = region
    pts = range ((0, 0), (x, y))


addPixel :: Grid -> (Int,Int) -> Grid
addPixel g (x,y) = undefined


expand :: (Point, Point) -> (Point, Point)
expand (a, b) = (a, b ^+^ V2 2 2)


neighbors :: Point -> [Point]
neighbors p = [p ^+^ (V2 x y) | x <- [-1, 0, 1], y <- [-1, 0, 1]]


binToInt :: [Char] -> Int
binToInt bs = foldl' asB 0 $ reverse bs
  where
    asB acc d = (2 * acc) + (digitToInt d)
