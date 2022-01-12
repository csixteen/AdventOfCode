module AOC.Day22 where

import Data.Either
import Data.Text

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
  deriving (Eq,Show)


data Action = On | Off deriving (Eq,Show)


data Toggle = Toggle
  { cuboid :: Cuboid
  , action :: Action
  }
  deriving (Eq,Show)


-- ---------------
--     Parsers
-- ---------------


parseToggles :: Text -> [Toggle]
parseToggles = fromRight (error "Parsing error") . parse pToggles ""


pToggles :: Parser [Toggle]
pToggles = pToggle `sepBy1` newline


pToggle :: Parser Toggle
pToggle = undefined
