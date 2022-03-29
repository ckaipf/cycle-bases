module EdgeShort
  ( edgeShortCycles,
    shortestPaths,
    edgeShort
  ) where

import           Graph as G (Graph, Edge, Cycle)
import qualified Graph as G
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Vector   (Vector, (!))
import qualified Data.Vector   as V
import qualified FloydWarshall as F
import qualified Data.IntSet as I

shortestPaths :: Graph -> [[[Int]]]
shortestPaths g = [[ F.shortestPath x y d' | y <- vs ] | x <- vs ]
  where
    vs = S.toList $ G.vertices g
    d' = V.fromList $ map V.fromList d
    d = F.floydWarshall g

-- intersection of paths equals 1
checkPaths :: [Int] -> [Int] -> Bool
checkPaths p q | null p || null q = False
               | (null . tail) p || (null . tail) q = False
               | I.null $ I.deleteMin $ I.intersection (I.fromList p) (I.fromList q) = True
               | otherwise = False


zipConcat :: [[Int]] -> [[Int]] -> [([Int],[Int])]
zipConcat [] [] = []
zipConcat (p:ps) (q:qs)
  | checkPaths p q = (p,q) : zipConcat ps qs
  | otherwise = zipConcat ps qs

edgeShort :: [Edge] -> Vector [[Int]] -> [([Int],[Int])]
edgeShort es ps = concatMap (\e -> zipConcat (ps!(G.tail e)) (ps!(G.head e))) es


fromV :: Vector (Vector (Maybe Edge)) -> ([Int],[Int]) -> Set Edge
fromV a (p,q) = S.insert (f (head q, head p)) $ S.union b c

     where tuplify (x:y:[]) = (x,y):[]
           tuplify (x:y:ys) = (x,y):(tuplify (y:ys))

           c = S.fromList $ map f $ tuplify $ p
           b = S.fromList $ map (\(x,y) -> f (y,x)) $ tuplify $ q

           f (v,u) = case ((a!u)!v) of
                       Nothing -> error "edge not in graph"
                       Just e  -> e

edgeShortCycles :: Graph -> Set Cycle
edgeShortCycles g =
  S.map (G.Cycle . (G.Subgraph g ) . G.edgesIndices) $
  S.fromList $
  map (fromV a) $
  edgeShort (S.toList es) $
  V.fromList $
  shortestPaths g'
  where
    es = G.edges g
    a = V.fromList $ map V.fromList $ G.toAdjacencyMatrix g'
    es' = S.map G.inverse es
    g' = G.Graph (S.union es es') (G.vertices g)
