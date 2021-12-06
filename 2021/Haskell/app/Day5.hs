module Day5 where

import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Map.Strict as S


solve :: FilePath -> IO Int
solve fileName =
  do contents <- readFile fileName
     let diagram = buildDiagram $ lines contents
     return (countOverlaps diagram)


type Point   = (Int, Int)
type Segment = (Point, Point)


-- --------------------------------------
--     Segments and Points helpers


-- Takes a Segment (Point A, Point B) and generates all the points
-- between A and B, inclusive.
generatePoints :: Segment -> [Point]
generatePoints s@(a, b) = genPoints a
  where
    genPoints p@(x,y) | same p b = [b]
                      | otherwise  = (x,y) : genPoints (nextPoint p s)


-- Takes a point and a segment and returns the next point in that
-- segment.
nextPoint :: Point -> Segment -> Point
nextPoint (x,y) ((x1,y1),(x2,y2)) = (x+dx, y+dy)
  where
    increment n = if n < 0 then -1 else if n > 0 then 1 else 0
    (dx, dy) = (increment (x2-x1), increment (y2-y1))


-- Indicates whether two points are the same.
same :: Point -> Point -> Bool
same (x1,y1) (x2,y2) = x1 == x2 && y1 == y2


-- ----------------------------------------
--           Diagram helpers


countOverlaps :: M.Map Point Int -> Int
countOverlaps = length . filter ((>= 2) . snd) . S.assocs


buildDiagram :: [String] -> M.Map Point Int
buildDiagram xs =
  let raw      = map (map (splitOn ",")) $ map (splitOn " -> ") xs
      segments = map buildSegment raw
      points   = concat $ map generatePoints segments
  in addPoints points M.empty
  where
    buildSegment [[x1,y1],[x2,y2]] = ((readInt x1, readInt y1), (readInt x2, readInt y2))


addPoints :: [Point] -> M.Map Point Int -> M.Map Point Int
addPoints ps d = foldl' addPoint d ps
  where
    addPoint :: M.Map Point Int -> Point -> M.Map Point Int
    addPoint m p = S.insertWith (+) p 1 m


readInt :: String -> Int
readInt s = read s :: Int
