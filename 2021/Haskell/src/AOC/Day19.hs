module AOC.Day19 where

import Data.Either
import Data.Monoid
import Data.Text

import Data.MultiSet as M
import Linear (V3(..), (^+^), (^-^))
import Relude.File (readFileText)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text


solve :: IO (Int,Int)
solve =
  do scanners <- parseScanners <$> readFileText "data/day19_input.txt"
     return (1,1)


-- -----------------------------------------
--       Type declarations and helpers
-- -----------------------------------------


type Point = V3 Int
type Transform = Endo Point


data Scanner  = Scanner
  { scannerId :: Int
  , beacons   :: [Point]
  , transformation :: Endo Point
  , signature :: M.MultiSet Int
  }


dist :: Point -> Point -> Int
dist p1 p2 = x^2 + y^2 + z^2
  where
    V3 x y z = p1 ^-^ p2


sign :: [Point] -> M.MultiSet Int
sign bs = M.fromList [dist a b | a <- bs, b <- bs, a /= b]


couldMatch :: Scanner -> Scanner -> Bool
couldMatch s1 s2 = s >= (12 * 11) `div` 2
  where
    s = M.size $ M.intersection (signature s1) (signature s2)


{-
   * Rotations *

Reference: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Monoid.html#t:Endo
This allows us to apply a single rotation or a composition of rotations to a Vector:
 appEndo rX (V3 1 1 1) == V3 1 (-1) 1
 appEndo (rX <> rZ) (V3 1 1 2) == V3 (-1) (-2) 1
-}
nullT = Endo id
rX = Endo \(V3 x y z) -> V3 x (-z) y
rY = Endo \(V3 x y z) -> V3 z y (-x)
rZ = Endo \(V3 x y z) -> V3 (-y) x z


-- -----------------------------
--          Parsers
-- -----------------------------

parseScanners :: Text -> [Scanner]
parseScanners = fromRight (error "Parsing error") . parse pScanners ""


pScanners :: Parser [Scanner]
pScanners = pScanner `sepBy` many1 endOfLine


pScanner :: Parser Scanner
pScanner = mkScanner <$> pScannerID <*> pBeacons
  where
    mkScanner :: Int -> [Point] -> Scanner
    mkScanner sId beacons = Scanner { scannerId      = sId
                                    , beacons        = beacons
                                    , transformation = nullT
                                    , signature      = sign beacons
                                    }


pScannerID :: Parser Int
pScannerID = between (string "--- scanner ") (string " ---") pNumber


pBeacons :: Parser [Point]
pBeacons = pBeacon `sepBy` endOfLine


pBeacon :: Parser Point
pBeacon = V3 <$> pNumber
             <*> (char ',' >> pNumber)
             <*> (char ',' >> pNumber)


pNumber :: Parser Int
pNumber =
  do
    digits <- many1 digit
    return . readInt $ digits


readInt :: String -> Int
readInt = read
