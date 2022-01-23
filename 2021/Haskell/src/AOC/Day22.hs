module AOC.Day22 where

import Data.Either
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T

import Relude.File (readFileText)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text


solve :: IO (Int,Int)
solve =
  do toggles <- parseToggles <$> readFileText "data/day22_input.txt"
     return (1,1)


-- -------------------------------------
--     Type declarations and helpers
-- -------------------------------------

data Cuboid = Cuboid
  { minX :: Int
  , maxX :: Int
  , minY :: Int
  , maxY :: Int
  , minZ :: Int
  , maxZ :: Int
  }
  deriving (Eq,Ord,Show)


validCuboid :: Cuboid -> Bool
validCuboid Cuboid{..} = minX <= maxX &&
                         minY <= maxY &&
                         minZ <= maxZ


intersects :: Cuboid -> Cuboid -> Bool
intersects = undefined


data Action = On | Off deriving (Eq,Show)


data Toggle = Toggle
  { cuboid :: Cuboid
  , action :: Action
  }
  deriving (Eq,Show)


type Grid = S.Set Cuboid


points :: Cuboid -> Int
points Cuboid{..} = ((maxX - minX) + 1) *
                    ((maxY - minY) + 1) *
                    ((maxZ - minZ) + 1)


-- --------------------------
--    Solvers and helpers
-- --------------------------


part1 :: [Toggle] -> Int
part1 toggles = sum $ points <$> S.elems grid
  where
    grid      = step S.empty toggles'
    toggles'  = filter (fifties . cuboid) toggles
    fifties Cuboid{..} = minX >= -50 &&
                         maxX <= 50  &&
                         minY >= -50 &&
                         maxY <= 50  &&
                         minZ >= -50 &&
                         maxZ <= 50


step :: Grid -> [Toggle] -> Grid
step grid []              = grid
step grid (t@Toggle{..}:ts) = step grid'' ts
  where
    (cs, cs') = partition (intersects cuboid) $ S.elems grid
    grid'     = foldr S.insert S.empty cs'
    grid''    = step' t cs grid'


step' :: Toggle -> [Cuboid] -> Grid -> Grid
step' = undefined


explode :: Cuboid -> Grid -> Cuboid -> Grid
explode c1 acc c2 = undefined


-- ---------------
--     Parsers
-- ---------------


parseToggles :: T.Text -> [Toggle]
parseToggles = fromRight (error "Parsing error") . parse pToggles ""


pToggles :: Parser [Toggle]
pToggles = pToggle `sepEndBy1` newline


pToggle :: Parser Toggle
pToggle =
  do
    let pAction "on"  = On
        pAction "off" = Off
        pAction _     = error "Unknown action"
    action <- pAction <$> (try (string "on") <|> try (string "off"))
    string " x="
    minX <- pNumber
    string ".."
    maxX <- pNumber
    string ",y="
    minY <- pNumber
    string ".."
    maxY <- pNumber
    string ",z="
    minZ <- pNumber
    string ".."
    maxZ <- pNumber
    return $ Toggle (Cuboid {..}) action


pNumber :: Parser Int
pNumber =
  do
    minus  <- optionMaybe (char '-')
    digits <- many1 digit
    return . readInt $ maybe digits (:digits) minus


readInt :: String -> Int
readInt = read
