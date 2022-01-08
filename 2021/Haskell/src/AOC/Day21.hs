module AOC.Day21 where

import Data.List
import Data.Maybe


solve :: IO (Int,Int)
solve =
  do let board = mkBoard 3 7
         part1 = play board
     return (part1,1)


-- ---------------------------
--      Types and helpers
-- ---------------------------


type Position = Int
type Score    = Int


data Player = Player Position Score
  deriving (Show)


data PlayerT = One | Two
  deriving (Eq,Show)


data Board = Board
  { player1 :: Player
  , player2 :: Player
  , turn    :: PlayerT
  , tosses  :: Int
  }
  deriving (Show)


type State = (Int, Board)


mkBoard :: Position -> Position -> Board
mkBoard p1 p2 = Board { player1 = Player p1 0
                      , player2 = Player p2 0
                      , turn    = One
                      , tosses  = 0
                      }


-- ---------------------------
--     Solvers and helpers
-- ---------------------------


play :: Board -> Int
play b = sc * (tosses b')
  where
    Player pos sc = loser b'
    (_, b')       = fromJust $ find hasWinner (iterate step (1,b))


loser :: Board -> Player
loser Board{..} = if isWinner player1 then player2 else player1


isWinner :: Player -> Bool
isWinner (Player _ sc) = sc >= 1000


hasWinner :: State -> Bool
hasWinner (_, Board{..}) = isWinner player1 || isWinner player2


step :: State -> State
step (die, b@Board{..}) = (die', board')
  where
    (ds, die') = rollDie die
    board' = case turn of
               One -> updatePlayer1 ds b
               Two -> updatePlayer2 ds b


updatePlayer1 :: [Int] -> Board -> Board
updatePlayer1 ds Board{..} = Board { player1 = updatePlayer player1 ds
                                   , turn    = Two
                                   , tosses  = tosses + 3
                                   , ..}


updatePlayer2 :: [Int] -> Board -> Board
updatePlayer2 ds Board{..} = Board { player2 = updatePlayer player2 ds
                                   , turn    = One
                                   , tosses  = tosses + 3
                                   , ..}


updatePlayer :: Player -> [Int] -> Player
updatePlayer (Player pos sc) ds = Player pos' sc'
  where
    sc'  = sc + pos'
    pos' = (pos + (sum ds)) `_mod` 10


rollDie :: Int -> ([Int], Int)
rollDie n = (xs, n')
  where
    xs = (\x -> x `_mod` 100) <$> [n..n+2]
    n' = 1 + last xs


_mod :: Int -> Int -> Int
_mod a b = ((a-1) `mod` b) + 1
