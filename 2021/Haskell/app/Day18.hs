module Day18 where

import Prelude hiding (lines)

import Data.Either
import Data.List hiding (lines)
import Data.Text hiding (foldl1')
import Relude.File (readFileText)
import Relude.Functor.Fmap
import Relude.String.Conversion hiding (show)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text


solve :: IO (Int,Int)
solve =
  do numbers <- parseSFNumber <<$>> lines <$> readFileText "data/day18_input.txt"
     let
       part1 = magnitude $ foldl1' (+) numbers 
     return (part1,1)


-- ---------------------------
--      Type declarations
-- ---------------------------


data SFNumber = Num Int
              | Pair SFNumber SFNumber
              deriving (Eq)


instance Num SFNumber where
  (+)         = addNumbers
  (-)         = undefined
  (*)         = undefined
  abs         = id
  signum      = undefined
  fromInteger = undefined


-- --------------------
--       Solvers
-- --------------------


magnitude :: SFNumber -> Int
magnitude (Num i) = i
magnitude (Pair a b) = 3 * a' + 2 * b'
  where
    a' = magnitude a
    b' = magnitude b


addNumbers :: SFNumber -> SFNumber -> SFNumber
addNumbers = undefined


-- ---------------------
--        Parsers
-- ---------------------


parseSFNumber :: Text -> SFNumber
parseSFNumber = fromRight (error "Parsing error") . parse pSFNumber ""


pSFNumber :: Parser SFNumber
pSFNumber = try pPair <|> pNum


pPair :: Parser SFNumber
pPair =
  do
    char '['
    a <- pSFNumber
    char ','
    b <- pSFNumber
    char ']'
    return (Pair a b)


pNum :: Parser SFNumber
pNum = Num <$> (read . toString) <$> many1 digit
