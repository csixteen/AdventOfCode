module AOC.Day19 where

import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.Text hiding (filter,length,maximum,zip)

import qualified Data.MultiSet as M
import Linear (Additive, V3(..), (^+^), (^-^))
import Relude.File (readFileText)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text


solve :: IO (Int,Int)
solve =
  do scanners <- parseScanners <$> readFileText "data/day19_input.txt"
     let
       scanners' = process scanners
       part1     = (S.size . S.unions) $ (S.fromList . beacons) <$> scanners'
       origins   = origin <$> scanners'
       part2     = maximum [manhattan a b | a <- origins, b <- origins]
     return (part1,part2)


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


instance Eq Scanner where
  s1 == s2 = (scannerId s1) == (scannerId s2)


data Processor = Processor
  { processed   :: [Scanner]
  , ongoing     :: [Scanner]
  , unprocessed :: [Scanner]
  }


-- ----------------------------
--     Solvers and helpers
-- ----------------------------


-- Part 1
process :: [Scanner] -> [Scanner]
process (s:ss) = processed $ process' Processor { processed   = []
                                                , ongoing     = [s]
                                                , unprocessed = ss
                                                }


process' :: Processor -> Processor
process' p@Processor{ongoing=[], ..} = p 
process' p = (process' . step) p


step :: Processor -> Processor
step Processor{..} = Processor { processed   = c : processed
                               , ongoing     = cs <> ongoing'
                               , unprocessed = unprocessed'
                               }
  where
    (c : cs)     = ongoing
    matches      = filter (couldMatch c) unprocessed
    matches'     = filter (isJust . snd) $ zip matches $ (matchRotation c) <$> matches
    matches''    = (\(m, r) -> (m, fromJust r)) <$> matches'
    unprocessed' = unprocessed \\ (fst <$> matches'')
    ongoing'     = rotateS <$> matches''


rotateS :: (Scanner, Rotate) -> Scanner
rotateS (Scanner{..}, rot) = Scanner { beacons  = (appEndo rot) <$> beacons
                                     , rotation = rot
                                     , ..}


-- If two scanners share at least 66 distances between beacons,
-- then there is a chance that there is a match. The 66 is derived
-- from (12 * 11) / 2.
couldMatch :: Scanner -> Scanner -> Bool
couldMatch s1 s2 = s >= 66
  where
    s = M.size $ M.intersection (signature s1) (signature s2)


matchRotation :: Scanner -> Scanner -> Maybe Rotate
matchRotation s1 s2 = listToMaybe $ matchR s1 s2


-- Tries to find a rotation that makes Scanner 1 and Scanner 2
-- match. If no rotation is found, it returns an empty list.
matchR :: Scanner -> Scanner -> [Rotate]
matchR s1 s2 = do
  let
    beacons1 = beacons s1
    beacons2 = beacons s2
  r  <- rotations
  b1 <- beacons1
  b2 <- beacons2
  let
    t     = b1 ^-^ (appEndo r b2)
    rot   = rotate t
    newB2 = (appEndo (rot <> r)) <$> beacons2
  guard $ (length $ intersect beacons1 newB2) >= 12
  return (rot <> r)


-- -------------------------------
--      Rotations and helpers
-- -------------------------------


manhattan :: Point -> Point -> Int
manhattan v1 v2 = (abs x) + (abs y) + (abs z)
  where
    (V3 x y z) = v1 ^-^ v2


origin :: Scanner -> Point
origin Scanner{..} = appEndo rotation (V3 0 0 0)


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
pScanners = pScanner `sepBy1` newline


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
pScannerID = between (string "--- scanner ") (string " ---\n") pNumber


pBeacons :: Parser [Point]
pBeacons = pBeacon `sepEndBy1` newline


pBeacon :: Parser Point
pBeacon = V3 <$> pNumber
             <*> (char ',' >> pNumber)
             <*> (char ',' >> pNumber)


pNumber :: Parser Int
pNumber =
  do
    minus  <- optionMaybe (char '-')
    digits <- many1 digit
    return . readInt $ maybe digits (:digits) minus


sign :: [Point] -> M.MultiSet Int
sign bs = M.fromList [dist a b | a <- bs, b <- bs, a /= b]


readInt :: String -> Int
readInt = read
