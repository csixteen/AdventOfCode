module AOC.Day19 where

import Data.Either
import Data.Monoid
import Data.Text

import Data.MultiSet as M
import Linear (Additive, V3(..), (^+^), (^-^))
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


type Point  = V3 Int
type Rotate = Endo Point


data Scanner  = Scanner
  { scannerId :: Int
  , beacons   :: [Point]
  , rotation  :: Rotate
  , signature :: M.MultiSet Int
  }
  deriving (Show)


instance Show Rotate where
  show c = show $ appEndo c (V3 1 1 1)


-- ----------------------------
--     Solvers and helpers
-- ----------------------------


-- If two scanners share at least 66 distances between beacons,
-- then there is a chance that there is a match. The 66 is derived
-- from (12 * 11) / 2.
couldMatch :: Scanner -> Scanner -> Bool
couldMatch s1 s2 = s >= 66
  where
    s = M.size $ M.intersection (signature s1) (signature s2)


-- -------------------------------
--      Rotations and helpers
-- -------------------------------


-- Distance between two points in 3D space
dist :: Point -> Point -> Int
dist p1 p2 = x^2 + y^2 + z^2
  where
    V3 x y z = p1 ^-^ p2


{-
Reference: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Monoid.html#t:Endo
This allows us to apply a single rotation or a composition of rotations to a Vector:
 appEndo rX (V3 1 1 1) == V3 1 (-1) 1
 appEndo (rX <> rZ) (V3 1 1 2) == V3 (-1) (-2) 1
-}
-- No rotation. It helps in the composition, since rX <> nullT == rX
nullT :: Endo (V3 Int)
nullT = Endo id


-- 90 degrees rotation along the X axis
rX :: Endo (V3 Int)
rX = Endo \(V3 x y z) -> V3 x (-z) y


-- 90 degrees rotation along the Y axis
rY :: Endo (V3 Int)
rY = Endo \(V3 x y z) -> V3 z y (-x)


-- 90 degrees rotation along the Z axis
rZ :: Endo (V3 Int)
rZ = Endo \(V3 x y z) -> V3 (-y) x z


-- Returns 24 compositions of rotations in the 3 axis
rotations :: [Rotate]
rotations = [a <> b | a <- as, b <- bs]
  where
    as = [nullT, rY, rY <> rY, rY <> rY <> rY, rZ, rZ <> rZ <> rZ]
    bs = [nullT, rX, rX <> rX, rX <> rX <> rX]


rotate :: (Additive f, Num a) => f a -> Endo (f a)
rotate v = Endo (v ^+^)


-- -----------------------------------
--          Parsers and helpers
-- -----------------------------------

parseScanners :: Text -> [Scanner]
parseScanners = fromRight (error "Parsing error") . parse pScanners ""


pScanners :: Parser [Scanner]
pScanners = pScanner `sepBy` many1 endOfLine


pScanner :: Parser Scanner
pScanner = mkScanner <$> pScannerID <*> pBeacons
  where
    mkScanner :: Int -> [Point] -> Scanner
    mkScanner sId bcs = Scanner { scannerId = sId
                                    , beacons   = bcs
                                    , rotation  = nullT
                                    , signature = sign bcs
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


sign :: [Point] -> M.MultiSet Int
sign bs = M.fromList [dist a b | a <- bs, b <- bs, a /= b]


readInt :: String -> Int
readInt = read
