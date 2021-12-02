module Day2 where

import Data.List


type Vec = (Int, Int, Int)

data Dir = Up Int
         | Down Int
         | Forward Int
  deriving Show


solve :: FilePath -> IO Int
solve fileName =
  do directions <- directionsFromFile fileName
     let (h, d, _) = navigate (0, 0, 0) directions
     return (h*d)


-- Take a list of directions and an initial position
-- and returns the final position after having navigated
-- according to the directions.
navigate :: Vec -> [Dir] -> Vec
navigate pos dirs = foldl' move pos dirs


-- Takes a Vec that represents a position and a direction
-- and returns a new position.
move :: Vec -> Dir -> Vec
move (h, d, a) dir =
  case dir of
    Up n      -> (h, d, a-n)
    Down n    -> (h, d, a+n)
    Forward n -> (h+n, d+(a*n), a)



directionsFromFile :: FilePath -> IO [Dir]
directionsFromFile fileName =
  do contents <- readFile fileName
     let dirs = map toDir $ fmap words $ lines contents
     return dirs


toDir :: [String] -> Dir
toDir [d, n] =
  case d of
    "forward" -> Forward (read n :: Int)
    "up"      -> Up (read n :: Int)
    "down"    -> Down (read n :: Int)
    _         -> error "Unknown direction"
