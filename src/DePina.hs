{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module DePina where

import           Data.FiniteField
import qualified Data.List        as L
import           Data.Ord         (comparing)
import           Data.Ratio
import qualified Data.Set         as S
import           Data.Vector      (Vector, (!))
import qualified Data.Vector      as V
import           EdgeShort
import           Gauss
import           GHC.TypeLits     (KnownNat)
import           Graph            (Cycle, Edge, Graph)
import qualified Graph            as G

dot :: (Num a) => Vector a -> Vector a -> a
dot v u = sum $ V.zipWith (*) v u

scale :: Num a => a -> Vector a -> Vector a
scale e v = V.map (*e) v

isOrthogonal :: (Eq a, Num a) => Vector a -> Vector a -> Bool
isOrthogonal v u = (dot v u) == 0

-- the tasks are basically
-- find a cycle ci which satisfies <ci, si> not equal to 0
findNonOrthogonalCycle :: (Num a, Eq a) => Vector a -> [Vector a] -> (Maybe (Vector a), [Vector a])
findNonOrthogonalCycle s cycles = findAndDelete cycles [] s
  where
    findAndDelete [] acc s = (Nothing, acc)
    findAndDelete (c:cs) acc s
      | isOrthogonal c s = findAndDelete cs (acc ++ [c]) s
      | otherwise = (Just c, (acc ++ cs))

-- update si+1 in order to satisfy <ci, si+1> equal to 0
class (Num a, Ord a) => Orthogonalizeable a where
  orthogonalize :: Vector a -> Vector a -> Vector a -> Vector a

instance Orthogonalizeable (Ratio Integer) where
  orthogonalize s c s'  = s' - scale ((dot s' c) / (dot s c)) s

instance (KnownNat p) => Orthogonalizeable (PrimeField p) where
  orthogonalize s c s'  = s' - scale ((dot s' c) / (dot s c)) s

auxiliaryVectors :: (Num a) => Graph -> [Vector a]
auxiliaryVectors g = [ V.generate (length es) (\i -> if j == i then 1 else 0)
  | j <- L.sort . map (flip (-) 1 . G.index) $ es ]
  where es = S.toList $ G.edges g

dePina :: (Orthogonalizeable a) => Graph -> (Integer -> a) -> [Cycle]
dePina g f = map (G.Cycle . (G.fromIncidenceVector g)) $ go [] cs ss
  where
    ss = map (V.map f) . auxiliaryVectors $ g
    n = S.size . G.edges $ g
    cs = map ((V.map f) . G.toIncidenceVector . G.subgraph) $
         L.sortBy (comparing G.length) . S.toList . edgeShortCycles $ g
    go acc cs (s:ss)
      | null ss   = acc'
      | otherwise = go acc' cs'
          (map
             (\s' ->
                if not $ isOrthogonal c s'
                  then orthogonalize s c s'
                  else s')
             ss)
      where
        (c, cs', acc') = case findNonOrthogonalCycle s cs of
                           (Nothing, cs') -> (V.empty, cs', acc)
                           (Just x, cs')  -> (x, cs', x:acc)
