{-# LANGUAGE DerivingVia #-}

module Day16 where

import Control.Monad
import Data.Either
import qualified Data.Text as T
import Relude.Bool.Guard (guarded)
import Relude.Extra.Newtype (un)
import Relude.File (readFileText)
import Relude.String.Conversion hiding (show)
import Text.Parsec.Combinator
import Text.Parsec.Pos (SourcePos,incSourceColumn,sourceColumn)
import Text.Parsec.Prim

import qualified Text.Show as S

solve :: IO (Int,Int)
solve =
  do packet <- hexToBin . (!!0) . T.lines <$> readFileText "data/day16_input.txt"
     let
       part1 = sumVersions $ parsePacket packet
       part2 = calculate $ parsePacket packet
     return (part1,part2)


-- ------------------------
--    Type declarations
-- ------------------------


newtype Version  = Version Int deriving (Show)

data Binary = Zero | One deriving (Eq,Ord,Show)
type BinStream = [Binary]
newtype BinNumber = BN BinStream deriving (Monoid,Semigroup) via BinStream

data Op = OpSum | OpProd | OpMin | OpMax | OpGT | OpLT | OpEQ deriving (Show)

data Packet = Literal Version BinNumber
            | Operator Version Op [Packet]

type HexNum = T.Text


-- -------------------------
--     Type conversions
-- -------------------------


binToInt :: Num a => BinNumber -> a
binToInt n = bti (un n) 0
  where
    bti []          acc = acc
    bti [Zero]      acc = acc
    bti [One]       acc = 1 + acc
    bti (Zero : bs) acc = bti bs (2*acc)
    bti (One : bs)  acc = bti bs (2*(acc+1))


intToBin :: Integral a => a -> BinNumber
intToBin n = BN (itb n [])
  where
    itb 0 acc = Zero : acc
    itb 1 acc = One : acc
    itb x acc = case x `divMod` 2 of
      (q,0) -> itb q (Zero : acc)
      (q,1) -> itb q (One : acc)
      _     -> error "Can't happen."


hexToBin :: HexNum -> BinStream
hexToBin = toString >=> (un . parseBin)
  where
    parseBin :: Char -> BinStream
    parseBin 'A' = un $ intToBin 10
    parseBin 'B' = un $ intToBin 11
    parseBin 'C' = un $ intToBin 12
    parseBin 'D' = un $ intToBin 13
    parseBin 'E' = un $ intToBin 14
    parseBin 'F' = un $ intToBin 15
    parseBin x   = padding . un . intToBin . readInt . toText $ ([x] :: String)
      where padding xs = replicate (4 - length xs) Zero <> xs
            readInt = read . toString


intToOp :: Int -> Op
intToOp 0 = OpSum
intToOp 1 = OpProd
intToOp 2 = OpMin
intToOp 3 = OpMax
intToOp 5 = OpGT
intToOp 6 = OpLT
intToOp 7 = OpEQ
intOpOp _ = error "Invalid operator"


-- ---------------------------------
--        Parsers and Helpers
--
-- References:
-- - https://hackage.haskell.org/package/parsec-3.1.15.0
-- - Real-world Haskell (Chapter 16)
-- - https://github.com/sonowz/advent-of-code-haskell/
-- ---------------------------------


parsePacket :: BinStream -> Packet
parsePacket ps = fromRight (error "Parsing error") $ parse pAlignedPacket "" ps


-- 4-bit alignment
pAlignedPacket :: Parsec BinStream () Packet
pAlignedPacket =
  do packet <- pUnalignedPacket
     column <- sourceColumn <$> getPosition :: Parsec BinStream () Int
     let padding = (column - 1) `mod` 4
     replicateM_ padding pZero
     return packet


-- Unaligned packet
pUnalignedPacket :: Parsec BinStream () Packet
pUnalignedPacket =
  do version <- Version . binToInt <$> pBinNumber 3
     pType   <- binToInt <$> pBinNumber 3 :: Parsec BinStream () Int
     if pType == 4 then pLiteralPacket version
     else pOperatorPacket version pType


pLiteralPacket :: Version -> Parsec BinStream () Packet
pLiteralPacket v = Literal v . BN <$> pValue
  where
    pValue :: Parsec BinStream () BinStream
    pValue = pBinStream 5 >>= (\xs -> case xs of
                                  Zero : value -> return value
                                  One : value  -> (<>) value <$> pValue
                                  []           -> error "Impossible")


pOperatorPacket :: Version -> Int -> Parsec BinStream () Packet
pOperatorPacket v t =
  do
    subpackets <- try pTotalLength <|> try pNumberSubpackets
    return (Operator v (intToOp t) subpackets)
  where
    pNumberSubpackets =
      do
        pOne
        count <- binToInt <$> pBinNumber 11
        replicateM count pUnalignedPacket
    pTotalLength =
      do
        pZero
        len   <- binToInt <$> pBinNumber 15
        start <- getColumn
        whileM ((\end -> end < start + len) <$> getColumn) pUnalignedPacket
    getColumn = sourceColumn <$> getPosition :: Parsec BinStream () Int
    whileM :: Monad m => m Bool -> m a -> m [a]
    whileM pred action =
      -- This could be written like this:
      -- pred >>= (\b -> if b then action >>= \a -> (a :) <$> (whileM pred action) else return []
      do p <- pred
         if p then do a <- action
                      (a :) <$> (whileM pred action)
         else return []


pZero :: Parsec BinStream () Binary
pZero = tokenPrim show incPos (guarded (== Zero))


pOne :: Parsec BinStream () Binary
pOne = tokenPrim show incPos (guarded (== One))


pBinary :: Parsec BinStream () Binary
pBinary = tokenPrim show incPos Just


pBinNumber :: Int -> Parsec BinStream () BinNumber
pBinNumber = (BN <$>) . pBinStream


pBinStream :: Int -> Parsec BinStream () BinStream
pBinStream n =
  do bs <- count n pBinary
     guard (length bs == n)
     return bs


incPos :: SourcePos -> Binary -> BinStream -> SourcePos
incPos p _ _ = incSourceColumn p 1


-- ---------------------
--       Solvers
-- ---------------------


sumVersions :: Packet -> Int
sumVersions (Literal (Version v) _)     = v
sumVersions (Operator (Version v) _ ps) = v + sum (sumVersions <$> ps)


calculate :: Packet -> Int
calculate (Literal  _ value)         = binToInt value
calculate (Operator _ op subpackets) = eval op (calculate <$> subpackets)


eval :: Op -> [Int] -> Int
eval OpSum   xs = sum xs
eval OpProd  xs = product xs
eval OpMin xs   = minimum xs
eval OpMax xs   = maximum xs
eval OpGT [a,b] = if a > b then 1 else 0
eval OpLT [a,b] = if a < b then 1 else 0
eval OpEQ [a,b] = if a == b then 1 else 0
eval _ _        = error "Undefined behavior"
