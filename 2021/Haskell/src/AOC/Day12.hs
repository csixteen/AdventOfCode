module AOC.Day12 where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       g = graph $ lines contents
       allPaths = paths g "start" "end"
       part2 = length allPaths
     return (1,part2)


type Graph   = M.Map Vertex [Vertex]
type Vertex  = String
type Path    = [Vertex]
type Visited = M.Map Vertex Int


graph :: [String] -> Graph
graph edges = foldl' addEdges M.empty edges'
  where
    addEdges acc [s,t] = addEdge acc s t
    edges' = fmap (splitOn "-") edges


addEdge :: Graph -> Vertex -> Vertex -> Graph
addEdge g s t = M.insertWith (++) t [s] g'
  where
    g' = M.insertWith (++) s [t] g


paths :: Graph -> Vertex -> Vertex -> [Path]
paths g s t = paths' g s t M.empty


paths' :: Graph -> Vertex -> Vertex -> Visited -> [Path]
paths' g s t vis
  | s == t            = [[t]]
  | not $ valid vis s = []
  | otherwise         =
    case filter (valid vis) $ g M.! s of
      [] -> []
      xs -> let vis' = visit vis s
                pss = foldl' (\acc s' -> acc ++ (paths' g s' t vis')) [] xs
            in fmap (s:) pss


special :: Vertex -> Bool
special v = v `elem` ["start", "end"]


bigCave :: Vertex -> Bool
bigCave = all isUpper


valid :: Visited -> Vertex -> Bool
valid vis v | bigCave v                         = True
            | not $ M.member v vis              = True
            | special v                         = False
            | (length $ M.filter (>1) vis) == 0 = True
            | otherwise                         = False


visit :: Visited -> Vertex -> Visited
visit vis v | bigCave v = vis
            | otherwise = M.insertWith (+) v 1 vis
