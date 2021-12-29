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
  deriving (Show)


instance Show Transform where
  show c = show $ appEndo c (V3 0 0 0)


nullT = Endo id


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


sign = undefined


readInt :: String -> Int
readInt = read
