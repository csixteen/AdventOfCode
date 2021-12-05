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
generatePoints s =
  if isHorizontal s then horPoints s
  else if isVertical s then verPoints s
  else []


verPoints :: Segment -> [Point]
verPoints s = verPoints' a b
  where
    (a, b) = flipVer s
    verPoints' p1@(x1,y1) p2 | same p1 p2 = [p2]
                             | otherwise  = (x1,y1) : verPoints' (x1,y1+1) p2


horPoints :: Segment -> [Point]
horPoints s = horPoints' a b
  where
    (a, b) = flipHor s
    horPoints' p1@(x1,y1) p2 | same p1 p2 = [p2]
                             | otherwise  = (x1,y1) : horPoints' (x1+1,y1) p2


same :: Point -> Point -> Bool
same (x1,y1) (x2,y2) = x1 == x2 && y1 == y2


isHorizontal :: Segment -> Bool
isHorizontal ((_,y1),(_,y2)) = y1 == y2


isVertical :: Segment -> Bool
isVertical ((x1,_),(x2,_)) = x1 == x2


flipHor :: Segment -> Segment
flipHor s@((x1,y1),(x2,y2)) =
  if x1 < x2 then s else ((x2,y1),(x1,y2))


flipVer :: Segment -> Segment
flipVer s@((x1,y1),(x2,y2)) =
  if y1 < y2 then s else ((x1,y2),(x2,y1))


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
