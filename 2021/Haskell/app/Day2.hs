module Day2 where

import Data.List


 -- (Horizontal position, depth, aim)
type Vec = (Int, Int, Int)

data Dir = Up Int
         | Down Int
         | Forward Int
  deriving Show


solve :: FilePath -> (Vec -> Dir -> Vec) -> IO Int
solve fileName f =
  do directions <- directionsFromFile fileName
     let (h, d, _) = navigate (0, 0, 0) directions f
     return (h*d)


-- Takes a list of directions and an initial position
-- and returns the final position after having navigated
-- according to the directions.
navigate :: Vec -> [Dir] -> (Vec -> Dir -> Vec) -> Vec
navigate pos dirs f = foldl' f pos dirs


-- Takes a Vec that represents a position and a direction
-- and returns a new position (part 1)
move_depth :: Vec -> Dir -> Vec
move_depth (h, d, a) dir =
  case dir of
    Up n      -> (h,   d-n, a)
    Down n    -> (h,   d+n, a)
    Forward n -> (h+n, d,   a)


-- Takes a Vec that represents a position and a direction
-- and returns a new position (part 2)
move_aim :: Vec -> Dir -> Vec
move_aim (h, d, a) dir =
  case dir of
    Up n      -> (h,   d,       a-n)
    Down n    -> (h,   d,       a+n)
    Forward n -> (h+n, d+(a*n), a)


-- -----------------------------------------------------
--                   Helpers

-- Takes a file path an returns a list of directions.

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
toDir _ = error "Bad input!"
