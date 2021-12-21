module Day17 where

import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Text as T (Text,lines,unlines)
import Relude.File (readFileText)
import Relude.String.Conversion hiding (show)
import Text.Parsec.Char
import Text.Parsec.Combinator (many1, optionMaybe)
import Text.Parsec.Prim (parse)
import Text.Parsec.Text


solve :: IO (Int,Int)
solve =
  do targetArea <- parseTargetArea <$> T.lines <$> readFileText  "data/day17_input.txt" :: IO TargetArea
     let
       part1 = highestY targetArea
       part2 = countVelocities targetArea
     return (part1,part2)


-- ---------------------------
--      Type declarations
-- ---------------------------

data TargetArea = TargetArea Int Int Int Int

newtype Pos = Pos (Int,Int)
newtype Velocity = Velocity (Int,Int)


-- ------------------
--      Solvers
-- ------------------


highestY :: TargetArea -> Int
highestY ta = case find (canReachArea ta) (velocities ta) of
  Just (Velocity(_,y)) -> sum [1..y]
  Nothing              -> error "Impossible!"


canReachArea :: TargetArea -> Velocity -> Bool
canReachArea ta@(TargetArea minX maxX minY maxY) v = inArea (last ps) ta
  where
    ps = takeWhile (\(Pos (x,y)) -> x <= maxX && y >= minY) $ fireProbe v (Pos (0,0))


-- Given an initial velocity and initial position, it generates an
-- infinite list with all the positions calculated according to the
-- stepping rules.
fireProbe :: Velocity -> Pos -> [Pos]
fireProbe v@(Velocity (dx,dy)) p@(Pos (x,y)) = p : fireProbe newVel newPos
  where
    newPos = move p v
    newVel = Velocity (max (dx-1) 0, dy-1)


move :: Pos -> Velocity -> Pos
move (Pos (x,y)) (Velocity (a,b)) = Pos (x+a, y+b)


inArea :: Pos -> TargetArea -> Bool
inArea (Pos (x,y)) (TargetArea x1 x2 y1 y2) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2


countVelocities :: TargetArea -> Int
countVelocities ta = length $ filter (canReachArea ta) (velocities ta)


velocities :: TargetArea -> [Velocity]
velocities (TargetArea _ maxX minY _) =
  [Velocity (x,y) | x <- [1..maxX], y <- reverse [(-maxY)..maxY]]
  where maxY = abs minY


-- --------------------
--       Parsers
-- --------------------


parseTargetArea :: [T.Text] -> TargetArea
parseTargetArea text =
  fromRight (error "parsing error") $
  parse pTargetArea "" (T.unlines text)


pNumber :: Parser Int
pNumber =
  do
    minus  <- optionMaybe (char '-')
    digits <- many1 digit
    return . readInt . toText $ maybe digits (: digits) minus
  where
    readInt = read . toString


pTargetArea :: Parser TargetArea
pTargetArea =
  TargetArea <$> (string "target area: x=" >> pNumber)
             <*> (string ".." >> pNumber)
             <*> (string ", y=" >> pNumber)
             <*> (string ".." >> pNumber)
