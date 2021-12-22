module Day18 where

import Prelude hiding (lines)

import Control.Applicative
import Data.Either
import Data.List hiding (lines)
import Data.Maybe
import Data.Text hiding (foldl1',maximum)
import Relude.File (readFileText)
import Relude.Functor.Fmap
import Relude.String.Conversion hiding (show)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Text


solve :: IO (Int,Int)
solve =
  do numbers <- parseSFNumber <<$>> lines <$> readFileText "data/day18_input.txt"
     let
       part1 = magnitude $ foldl1' addNumbers numbers
       part2 = maximum $ magnitudes numbers
     return (part1,part2)


-- ---------------------------
--      Type declarations
-- ---------------------------


data SFNumber = Num Int
              | Pair SFNumber SFNumber
              deriving (Eq)


-- --------------------
--       Solvers
-- --------------------


magnitudes :: [SFNumber] -> [Int]
magnitudes ns = [(magnitude . uncurry addNumbers) (a,b) | a <- ns, b <- ns, a /= b]


magnitude :: SFNumber -> Int
magnitude (Num i) = i
magnitude (Pair a b) = 3 * a' + 2 * b'
  where
    a' = magnitude a
    b' = magnitude b


addNumbers :: SFNumber -> SFNumber -> SFNumber
addNumbers a b = reduce $ Pair a b


reduce :: SFNumber -> SFNumber
reduce n = maybe n reduce (explodeNumber n <|> splitNumber n)


explodeNumber :: SFNumber -> Maybe SFNumber
explodeNumber n = fst <$> explode 0 n 


explode :: Int -> SFNumber -> Maybe (SFNumber, (Int,Int))
explode _ (Num _) = Nothing
explode depth (Pair (Num l) (Num r)) | depth >= 4 = Just (Num 0, (l, r))
explode depth (Pair left right)  = (explodeLeft right <$> left') <|> (explodeRight left <$> right')
  where
    left'  = explode (depth + 1) left
    right' = explode (depth + 1) right


explodeLeft :: SFNumber -> (SFNumber, (Int,Int))-> (SFNumber, (Int,Int))
explodeLeft rightmost (num, (left, right)) = (Pair num (addLeft right rightmost), (left,0))
  where
    addLeft :: Int -> SFNumber -> SFNumber
    addLeft n (Num x)    = Num (x+n)
    addLeft n (Pair l r) = Pair (addLeft n l) r


explodeRight :: SFNumber -> (SFNumber, (Int,Int)) -> (SFNumber, (Int,Int))
explodeRight leftmost (num, (left, right)) = (Pair (addRight left leftmost) num, (0,right))
  where
    addRight :: Int -> SFNumber -> SFNumber
    addRight n (Num x)    = Num (x+n)
    addRight n (Pair l r) = Pair l (addRight n r)


splitNumber :: SFNumber -> Maybe SFNumber
splitNumber (Num n)
  | n < 10    = Nothing
  | otherwise = Just $ Pair (Num q) (Num $ q + r)
  where
    (q,r) = n `divMod` 2
splitNumber (Pair left right) = left' <|> right'
  where
    left'  = flip Pair right <$> splitNumber left
    right' = Pair left <$> splitNumber right 


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
