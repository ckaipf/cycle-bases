module FloydWarshall
  (
    floydWarshall,
{- returns DP matrix -}

    shortestPath
{- backtracing of DP matrix-}
  ) where

{- Algebraic Floyd–Warshall from Jeremy T. Fineman and Eric Robinson (Graph G.gorithms in the Language of Linear G.gebra. 2011, 45-58)
Positiv and negative edge weights (no negative cycles) O(N^3) -}

import           Graph (Edge, Graph, Weight)
import qualified Graph as G
import           Control.Applicative
import qualified Data.List           as L
import           Data.Semigroup
import qualified Data.Set            as S
import Data.Vector (Vector, (!))

data WPP = WPP { weight :: Weight, parentPointer :: Int } deriving (Read, Show, Eq)

instance Num WPP where
  x + y = WPP (weight x + weight y) (parentPointer y)


instance Ord WPP where
  min x y = if weight x < weight y then x else y

instance Semigroup WPP where
  x <> y = min x y

nestedZipWith :: (a -> a -> a) -> [[a]] -> [[a]] -> [[a]]
nestedZipWith = zipWith . zipWith

outerProduct :: (a -> a -> a) -> [a] -> [a] -> [[a]]
outerProduct f v u = [[ f x y | y <- u ] | x <- v ]

initialize :: Graph -> [[Maybe WPP]]
initialize g = map (map f) a
  where a = G.toAdjacencyMatrix g
        f x = case x of
                Nothing -> Nothing
                Just x  -> Just $ WPP (G.weight x) (G.tail x)

floydWarshall :: Graph -> [[Maybe WPP]]
floydWarshall g = L.foldl' f (initialize g) $ (S.toList . G.vertices) g
  where f d v = nestedZipWith (<>) d d'
          where d' = outerProduct (liftA2 (+)) (map (!! v) d) (d!!v)

shortestPath :: Int -> Int -> Vector (Vector (Maybe WPP)) -> [Int]
shortestPath s t d = go [] t
  where u = d ! s
        go acc v = if s == v then v:acc
                   else let in case u ! v of
                                 Nothing -> s:v:acc
                                 Just b -> go (v:acc) (parentPointer b)

