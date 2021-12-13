module Day12 where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set as S


solve :: FilePath -> IO (Int,Int)
solve fileName =
  do contents <- readFile fileName
     let
       g = graph $ lines contents
       allPaths = paths g "start" "end"
       part1 = length allPaths
     return (part1,1)


type Graph   = M.Map Vertex [Vertex]
type Vertex  = String
type Path    = [Vertex]
type Visited = S.Set Vertex


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
paths g s t = paths' g s t S.empty


paths' :: Graph -> Vertex -> Vertex -> Visited -> [Path]
paths' g s t vis
  | s == t = [[s]]
  | otherwise =
    case filter (\t' -> not $ S.member t' vis) $ g M.! s of
      [] -> []
      xs ->
        let
          vis' = visit vis s
          pss = foldl' (\acc s' -> acc ++ (paths' g s' t vis')) [] xs
        in
          fmap (s:) pss


visit :: Visited -> Vertex -> Visited
visit vis v | all isUpper v = vis
            | otherwise     = S.insert v vis
