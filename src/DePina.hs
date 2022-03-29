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

{-
  Input: Two vectors to calculate a 'dot product' (technically not because it is not positive definite)
  Output: The sum of the product of the components of the vectors
  
  Function to calculate the inner product of two vectors as sum of the products of their components
-}
dot :: (Num a) => Vector a -> Vector a -> a
dot v u = sum $ V.zipWith (*) v u

{-
  Input: A scaling factor and a vector to scale
  Output: The scaled vector
  
  Function to scale a vector by an element of the field of interest
-}
scale :: Num a => a -> Vector a -> Vector a
scale e v = V.map (*e) v

{-
  Input: Two vectors to determine orthogonality
  Output: True when input vectors are orthogonal, False otherwise
  
  Checks whether input vectors are orthogonal (inner product vanishes)
-}
isOrthogonal :: (Eq a, Num a) => Vector a -> Vector a -> Bool
isOrthogonal v u = (dot v u) == 0

{-
  Input: The witness vector and the set of candidate cycles among whose to find a non-orthogonal one
  Output: The first cycle not orthogonal to the witness vector and all other cycles
  
  Function to find a cycle that is not orthogonal to the witness vector s provided (if any).
  Will go through the cycles from the candidate set until it finds 
  a cycle that is not orthogonal to s (i.e. <c,s> != 0, that is a non-vanishing inner product),
  and return it and a list of all other cycles. 
-}
findNonOrthogonalCycle :: (Num a, Eq a) => Vector a -> [Vector a] -> (Maybe (Vector a), [Vector a])
-- non-orthogonal cycle is found by findAndDelete recursion, finding a non-orthogonal one and collecting all others 
-- (deleting the non-orthogonal one from the list of all cycles)
findNonOrthogonalCycle s cycles = findAndDelete cycles [] s
  where
    -- in the base case (no cycles remain), the acc (as filled in other case, i.e. all) is returned
    findAndDelete [] acc s = (Nothing, acc)
    -- if cycles remain, consider the first cycle c (head of the list)
    findAndDelete (c:cs) acc s
        -- if current cycle is orthogonal to s, add it to the accumulator and keep on looking
      | isOrthogonal c s = findAndDelete cs (acc ++ [c]) s
        -- if a cycle with the desired property is found, return it and all other cycles
        -- (i.e. the cycles considered before and the cycles not yet considered)
      | otherwise = (Just c, (acc ++ cs))

-- For an instance of type a to be an Orthogonalizeable, it needs to support the orthogonalize class method
class (Num a, Ord a) => Orthogonalizeable a where
  orthogonalize :: Vector a -> Vector a -> Vector a -> Vector a

-- Making a Ratio Integer an orthoganizable class by subtracting the non-orthogonal components from it
instance Orthogonalizeable (Ratio Integer) where
  orthogonalize s c s'  = s' - scale ((dot s' c) / (dot s c)) s

-- Making a prime field an orthoganizable class by subtracting the non-orthogonal components from it
instance (KnownNat p) => Orthogonalizeable (PrimeField p) where
  orthogonalize s c s'  = s' - scale ((dot s' c) / (dot s c)) s

{-
  Input: A graph to extract auxiliary vectors from
  Output: The auxiliary vectors corresponding to the graph
  
  Function to extract auxiliary vectors used for the orthogonality tests within dePina
-}
auxiliaryVectors :: (Num a) => Graph -> [Vector a]
-- construct a list of unit vectors and ... with the indices of the edges of the graph
auxiliaryVectors g = [ V.generate (length es) (\i -> if j == i then 1 else 0)
  | j <- L.sort . map (flip (-) 1 . G.index) $ es ]
        -- extract the edges of the graph and listify them
  where es = S.toList $ G.edges g

{-
  Input: A graph to derive a MCB from and a field map for VR coefficients
  Output: An MCB of the graph
  
  Function to derive a minimal cycle base from a graph using dePinas algorithm.
  Will find a MCB with orthogonal cycles, using a set of (dynamic) auxiliary vectors
-}
dePina :: (Orthogonalizeable a) => Graph -> (Integer -> a) -> [Cycle]
-- will return the cycles found through recursion of go
dePina g f = map (G.Cycle . (G.fromIncidenceVector g)) $ go [] cs ss
  where
    -- field-mapped auxiliary vectors of the input graph
    ss = map (V.map f) . auxiliaryVectors $ g
    -- #edges
    n = S.size . G.edges $ g
    -- field-mapped edge short cycles of g, sorted by length
    cs = map ((V.map f) . G.toIncidenceVector . G.subgraph) $
         L.sortBy (comparing G.length) . S.toList . edgeShortCycles $ g
    -- recursion to orthogonize the vectors     
    go acc cs (s:ss)
        -- when the last auxiliary vector is about to be processed, return the new MCB
      | null ss   = acc'
        -- if there are still auxiliary vectors to process, continue the recursion with the extended accumulator and the reduced cycle set
      | otherwise = go acc' cs'
          -- and the processed auxiliary vectors
          (map
             (\s' ->
                -- by orthogonalizing each remaining auxiliary vector 
                -- with the current auxiliary vector and (first) cycle not orthogonal to the current auxiliary vector
                if not $ isOrthogonal c s'
                  then orthogonalize s c s'
                  -- if its already orthogonal to the first non-orthorgonal cycle do nothing
                  else s')
             ss)
      where
        (c, cs', acc') = case findNonOrthogonalCycle s cs of
                           -- if cycle set only contains cycles orthogonal to the auxiliary vector, there's nothing to do
                           (Nothing, cs') -> (V.empty, cs', acc)
                           -- otherwise identify first non-orthorgonal cycle, remove it from the cycle set and add it to the accumulator (partial MCB)
                           (Just x, cs')  -> (x, cs', x:acc)
