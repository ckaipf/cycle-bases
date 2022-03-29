{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph (
  Weight (..),

  Graph (..),

  Edge (..),
  fromAdjacencyList,
  edgesToVertices,
  inverse,
  toAdjacencyMatrix,
  edgesIndices,
  edgeIndicesToEdges,
  esSum,

  Subgraph (..),
  toIncidenceVector,
  fromIncidenceVector,

  Cycle (..),
  fromVerticesList,
  mapOverEdgeLabels,
  length

  ) where

import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as I
import qualified Data.List           as L
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V
import           Prelude             hiding (head, tail, length)
import qualified Prelude             as P
import qualified Data.Foldable       as F
import           Data.Monoid

{- -}
newtype Weight = Weight Float deriving (Ord, Eq, Read, Num, Show)

data Edge = Edge { tail :: Int, head :: Int, index :: Int, weight :: Weight } deriving (Ord, Eq, Read, Show)

inverse :: Edge -> Edge
inverse e = Edge (head e) (tail e) ((index e)*(-1)) (weight e)

edgesToVertices :: Set Edge -> Set Int
edgesToVertices es = S.union (S.map tail es) (S.map head es)

esSum :: Set Edge -> Weight
esSum es = getSum $ F.foldMap (Sum . weight) es

edgesIndices :: Set (Edge) -> IntSet
edgesIndices es = I.fromList $ S.toList $ S.map index es

{- -}
data Graph = Graph { edges :: Set (Edge), vertices :: Set Int } deriving (Read, Show)

toAdjacencyMatrix :: Graph -> [[Maybe (Edge)]]
toAdjacencyMatrix g = [[ f y x | x <- [0..n]] | y <- [0..n] ]
  where n = S.size $ vertices g
        f a b = if null es then Nothing else Just $ S.findMin es
          where es = S.filter (\e -> (tail e) == a && (head e) == b) $ edges g

fromAdjacencyList :: Vector [Edge] -> Graph
fromAdjacencyList al = Graph es vs
  where es = S.fromList $ concatMap id $ V.toList al
        vs = S.fromList $ concatMap (\e -> [head e, tail e]) es

edgeIndicesToEdges :: Graph -> IntSet -> Set (Edge)
edgeIndicesToEdges g is = S.filter (\e -> I.member (index e) is) $ edges g



{- -}
data Subgraph = Subgraph { supergraph :: Graph, edgeIndices :: IntSet } deriving (Read)

instance Eq (Subgraph) where
  a == b = (edgeIndices) a == (edgeIndices) b

instance Ord (Subgraph) where
  compare a b = compare ((edgeIndices) a) ((edgeIndices) b)

instance Show (Subgraph) where
  show a =  "(" ++ (show . I.toList . edgeIndices) a ++ ")"

toIncidenceVector :: (Num a) => Subgraph -> Vector a
toIncidenceVector g = V.generate n f
  where f i | I.member j (edgeIndices g)    = 1
            | I.member (-j) (edgeIndices g) = (-1)
            | otherwise             = 0
             where j = i + 1
        n = S.size $ edges $ supergraph g

fromIncidenceVector :: (Ord a,Num a) => Graph -> Vector a -> Subgraph
fromIncidenceVector g v = Subgraph g (I.union a b)
 where a  = I.fromList $ V.toList $ V.map (+1) $ V.findIndices (>0) v
       b  = I.fromList $ V.toList $ V.map (*(-1)) $ V.map (+1) $ V.findIndices (<0) v




{- -}
newtype Cycle = Cycle { subgraph :: Subgraph } deriving (Read, Eq, Ord)

instance Show (Cycle) where
  show a =  (show . subgraph) a

length :: Cycle -> Weight
length c = esSum $ edgeIndicesToEdges (supergraph $ subgraph c) $ edgeIndices $ subgraph c

mapOverEdgeLabels :: (Int -> Int) -> Cycle -> Cycle
mapOverEdgeLabels f c = Cycle (Subgraph (supergraph $ subgraph c) is')
  where is' = I.map f $ edgeIndices $ subgraph c

fromVerticesList :: Vector (Vector (Maybe Edge)) -> [Int] -> Set Edge
fromVerticesList a vs = S.fromList $ map f $ tuplify $ vs ++ [P.head vs]
     where tuplify (x:y:[]) = (x,y):[]
           tuplify (x:y:ys) = (x,y):(tuplify (y:ys))
           f (v,u) = case ((a!u)!v) of
                       Nothing -> error "edge not in graph"
                       Just e  -> e
