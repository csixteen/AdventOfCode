module AOC.Day21 where

import Control.Applicative
import Data.Ix
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe


solve :: IO (Int,Integer)
solve =
  do let board = mkBoard 3 7
         part1 = play board
         part2 = (uncurry max) $ universes board
     return (part1,part2)


-- ---------------------------
--      Types and helpers
-- ---------------------------


type Position = Int
type Score    = Int


data Player = Player Position Score
  deriving (Show)


data PlayerT = One | Two
  deriving (Eq, Show)


data Board = Board
  { player1 :: Player
  , player2 :: Player
  , turn    :: PlayerT
  , tosses  :: Int
  }
  deriving (Show)


type State = (Int, Board)
type Cache = M.Map String (Integer,Integer)
type Wins  = (Integer,Integer)


mkBoard :: Position -> Position -> Board
mkBoard p1 p2 = Board { player1 = Player p1 0
                      , player2 = Player p2 0
                      , turn    = One
                      , tosses  = 0
                      }


boardToStr :: Board -> String
boardToStr Board{..} = concat $ intersperse "-" $ [show player1, show player2, show turn]


-- ---------------------------
--     Solvers and helpers
-- ---------------------------


-- Part 2

universes :: Board -> Wins
universes b = fst $ universes' b M.empty


universes' :: Board -> Cache -> (Wins, Cache)
universes' b cache = case cache M.!? (boardToStr b) of
  Just v  -> (v, cache)
  Nothing -> universes'' b cache


universes'' :: Board -> Cache -> (Wins, Cache)
universes'' b cache = fromJust $
  (wins b cache) <|> (multiverse b cache)


multiverse :: Board -> Cache -> Maybe (Wins, Cache)
multiverse board cache = Just $ foldl' (mVerse board) ((0,0), cache) choices
  where
    mVerse :: Board -> (Wins,Cache) -> (Int,Int) -> (Wins,Cache)
    mVerse _board ((a,b), _cache) (c,n) =
      let
        ((a',b'), cache') = universes' (updateBoard _board c) _cache
        w = (a + a' * (toInteger n), b + b' * (toInteger n))
        cache'' = M.insert (boardToStr _board) w cache'
      in
        (w, cache'')


wins :: Board -> Cache -> Maybe (Wins,Cache)
wins b@Board{..} cache =
  case hasWinner (0, b) of
    False -> Nothing
    True  ->
      let
        Player _ sc1 = player1
        Player _ sc2 = player2
        w = (if sc1 > 20 then 1 else 0, if sc2 > 20 then 1 else 0)
        cache' = M.insert (boardToStr b) w cache
      in
        Just (w, cache')


choices :: [(Int,Int)]
choices = M.assocs m
  where
    m = foldl' addChoice M.empty $ range ((1,1,1), (3,3,3))
    addChoice acc choice = M.insertWith (+) (sum3 choice) 1 acc


sum3 :: (Int,Int,Int) -> Int
sum3 (a,b,c) = (a + b + c)


-- Part 1

play :: Board -> Int
play b = sc * (tosses b')
  where
    Player _ sc = loser b'
    (_, b')     = fromJust $ find hasWinner (iterate step (1,b))


loser :: Board -> Player
loser Board{..} = if isWinner player1 then player2 else player1


isWinner :: Player -> Bool
isWinner (Player _ sc) = sc >= 1000


hasWinner :: State -> Bool
hasWinner (_, Board{..}) = isWinner player1 || isWinner player2


step :: State -> State
step (die, b) = (die', updateBoard b $ sum ds)
  where (ds, die') = rollDie die


updateBoard :: Board -> Int -> Board
updateBoard b@Board{..} s =
  case turn of
    One -> updatePlayer1 s b
    Two -> updatePlayer2 s b


updatePlayer1 :: Int -> Board -> Board
updatePlayer1 s Board{..} = Board { player1 = updatePlayer player1 s
                                  , turn    = Two
                                  , tosses  = tosses + 3
                                  , ..}


updatePlayer2 :: Int -> Board -> Board
updatePlayer2 s Board{..} = Board { player2 = updatePlayer player2 s
                                  , turn    = One
                                  , tosses  = tosses + 3
                                  , ..}


updatePlayer :: Player -> Int -> Player
updatePlayer (Player pos sc) s = Player pos' sc'
  where
    sc'  = sc + pos'
    pos' = (pos + s) `_mod` 10


rollDie :: Int -> ([Int], Int)
rollDie n = (xs, n')
  where
    xs = (\x -> x `_mod` 100) <$> [n..n+2]
    n' = 1 + last xs


_mod :: Int -> Int -> Int
_mod a b = ((a-1) `mod` b) + 1
