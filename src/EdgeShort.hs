module EdgeShort
  ( edgeShortCycles,
    shortestPaths,
    edgeShort
  ) where

import           Graph as G (Graph, Edge, Cycle)
import qualified Graph as G
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Vector   ((!))
import qualified Data.Vector   as V
import qualified FloydWarshall as F
import qualified Data.IntSet as I

{-
  Input: A graph to calculate the shortest paths within
  Output: An array of lists of shortest paths between nodes
  
  Function to calculate the shortest paths within a graph.
  Based on the algorithm of FloydWarshall.
  Uses the WPP (weight parent-pointer) data structure (addition and comparison/order by weight, monoid operation by comparison).
-}
shortestPaths :: Graph -> [[[Int]]]
-- shortest path between x and y (based on WPP d) is saved in an array / a list of list
-- based on the shortestPath recursion algorithm in FloydWarshall.hs
shortestPaths g = [[ F.shortestPath x y d | y <- vs ] | x <- vs ]
  where
    -- list of vertices in the graph
    vs = S.toList $ G.vertices g
    -- array of WPP (if exists)
    d = F.floydWarshall g


{-
  Input: Two paths as lists of integers of the corresponding nodes
  Output: A boolean stating whether the paths share only one node
  
  Function to check whether two paths intersect in more than one node.
  If paths intersect at more than one node, (at least 1) is/are not edge short / chordless
-}
checkPaths :: [Int] -> [Int] -> Bool
checkPaths p q | length p <= 1 || length q <= 1 = False
                 -- only return true when the intersection contains exactly one node (always contains the 'final node')
               | ((==) 1) $ I.size $ I.intersection (I.fromList p) (I.fromList q) = True
               | otherwise = False

{-
  Input: Two lists of paths
  Output: A list of extended/unioned paths that share only one node
  
  Function to extend paths as two halves of a cycle (modulo a connecting edge)
-}
zipConcat :: [[Int]] -> [[Int]] -> [[Int]]
zipConcat [] [] = []
-- when there is some actual work to do
zipConcat (p:ps) (q:qs)
    -- when the paths only share one common node, concate the paths (module the shared node, and with one path in reverse)
    -- and cons it with other legit paths
  | checkPaths p q = (p ++ (tail $ reverse q)) : zipConcat ps qs
    -- if they share more than one node, ignore the paths
  | otherwise = zipConcat ps qs

{-
  Input: A set of edges and the array of WPP lists (shortest paths in FloydWarshall form)
  Output: A list of the edgeShort cycles within the graph corrsponding to the input edge set as list of vertex index tuples (tail & head)
  
  Function to calculate the edgeShort cycles as list of tuples of vertex indices (for edges in a cycle)
-}
edgeShort :: [Edge] -> [[[Int]]] -> [[Int]]
-- concatenates all edges in the input as tuples of the indices of their WPP lists
edgeShort es ps = concatMap (\e -> zipConcat (ps'!(G.tail e)) (ps'!(G.head e))) es
  -- listed WPPs
  where ps' = V.fromList ps

{-
  Input: The graph to extract the edge short cycles from
  Output: the set of edge-short cycles within the graph
  
  Function to extract all edgeShortCycles from a graph 
-}
edgeShortCycles :: Graph -> Set Cycle
edgeShortCycles g =
  -- parse edgeShorts to a cycle set
  S.fromList $
  -- parse the edgeShorts to cycles from the adjacency matrix (via subgraph, edgeIndices and vertex list)
  map (G.Cycle . (G.Subgraph g ) . G.edgesIndices . (G.fromVerticesList a)) $
  -- calculate the edgeShorts of the original edges based on the shortest paths derived
  edgeShort (S.toList es) $
  -- calculate the shortest paths of edges and inverse edges (lose directedness)
  shortestPaths g'
  where
    -- edges of the graph
    es = G.edges g
    -- listified adjecency matrix
    a = V.fromList $ map V.fromList $ G.toAdjacencyMatrix g'
    -- inverted edges of the graph (reversed edges)
    es' = S.map G.inverse es
    g' = G.Graph (S.union es es') (G.vertices g)
