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


generatePoints :: Segment -> [Point]
generatePoints (a@(x1,y1), b@(x2,y2)) = genPoints a
  where
    (h, v) = (x2-x1, y2-y1)
    genPoints p@(x,y) | same p b = [b]
                      | otherwise  = (x,y) : genPoints (nextPoint p h v)


nextPoint :: Point -> Int -> Int -> Point
nextPoint (x,y) h v = (x+dx, y+dy)
  where
    increment n = if n < 0 then -1 else if n > 0 then 1 else 0
    (dx, dy) = (increment h, increment v)


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
