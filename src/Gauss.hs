{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}

module Gauss
  ( extendBase,
    extractBase,
    Reducible,
    extractBaseEnum
  ) where

import           Data.FiniteField
import           Data.List               (foldl', sortBy)
import           Data.Ord                (Down)
import           Data.Vector             (Vector, (!))
import qualified Data.Vector             as V
import           GHC.TypeLits            (KnownNat)

-- For an instance of type a to be a Reducible, it needs to support the reduceVector class method
class (Num a, Ord a) =>
      Reducible a where
  -- a reduceVector class method uses a vector to reduce a(nother) vector and returns the reduced vector
  -- Within this context it is used to subtract linearly dependent component of the vector,
  -- based on the vector whose non-zero components should be reduced from the vector to reduce
  reduceVector :: Vector a -> Vector a -> Vector a

-- Making a prime field a reducible class by reducing the first non-zero component of the reduction vector
-- from the vector to reduce
instance (KnownNat p) => Reducible (PrimeField p) where
  -- reduce u by v through adding the k-multiple of it that the first non-zero index of v (i.e. i) vanishes in u
  reduceVector u v = u + k `scale` v
    where
      -- find the first non-zero index (if exists) of v
      i =
        case (V.findIndex (/= 0) v) of
          Nothing -> error "zero vector"
          Just x -> x
      -- find the multiple so the i-th component of u and v are inverse    
      k = (-(u ! i)) * (recip (v ! i))

-- Making an integer a reducible class by subtracting the scaled first non-zero component of the vector
-- to reduce with from the vector to reduce
instance Reducible Integer where
  -- reduce u by v through
  reduceVector u v
      -- if the component in u where v has its first non-zero component is 0, nothing needs to be done
    | u ! i == 0 = u
      -- if the component in u where v has its first non-zero component is non-zero, it needs to be reduced
      -- by subtracting the opposing quantity so that the ith component of the scaled vectors cancels out
    | otherwise =
      -- corresponding multiples with opposing sign are added so that the ith component of u becomes 0
      (m `div` (abs u ! i)) `scale` u + (s * (m `div` (abs v ! i))) `scale` v
    where
      -- index of where the first non-zero entry in v is located
      i =
        case (V.findIndex (/= 0) v) of
          Nothing -> error "zero vector"
          Just x -> x
      -- the least common multiple of the components where v has its first non-zero component
      m = lcm (u ! i) (v ! i)
      -- the sign for the reduction operation, making sure that the scaled qunatities at i cancel out
      s =
        if signum (u ! i) == signum (v ! i)
          -- if the sign of u and v at i is the same, they need to oppose
          then (-1)
          -- otherwise they are already (unscaled) inverses
          else 1

{-
  Functionality for simple vector arithmetics, applying the operator to each vector entry
-}
instance (Num a) => Num (Vector a) where
  x + y = V.zipWith (+) x y
  x - y = V.zipWith (-) x y
  abs x = V.map (abs) x

{-
  Input: A scaling factor and a vector to scale
  Output: The scaled vector
  
  Function to scale a vector by an element of the field of interest
-}
scale :: Num a => a -> Vector a -> Vector a
scale e v = V.map (*e) v

{-
  Input: a (partial) base and a vector to reduce
  Output: the reduction of the vector to reduce
  
  Function to successively reducing a vector by subtracting linearly dependent entries of the partial base from the vector.
  Implements a variant of Gauss, where the partial base (preferably in upper echolon form attempts to subtract its
  share (as a projection) from the vector to reduce / the attempt to extend the base
-}
extendBase :: (Reducible a) => [Vector a] -> Vector a -> Vector a
extendBase base v  = foldl' (reduceVector) v (sortBy absDescend base)

{-
  Input: list of (potentially dependent) vectors 
  Output: list of a linearly independent subset of the input vectors
  
  Function to extract a linearly independent set ((partial) base) from a set of vectors
  Done by recursively attempting to represent vectors as linear combinations of the set of (earlier in the call stack)
  provenly independent vectors. 
  Only vectors that can't be reduced to the 0-vector (i.e. written as linear combinations of the other vectors) are kept
  Basically exactly the same as extractBaseEnum, except that it doesn't keep track of the indices of the (reduced) vectors.
-}
extractBase :: (Reducible a) => [Vector a] -> [Vector a]
extractBase [] = []
-- filter out all vectors that can be reduced to 0 by adding linear combinations of other vectors
-- (if not keep the vector by consing it to the result set)
extractBase (v:base) = if V.all (==0) v' then base' else v' : base'
                       -- result of recursive calls of the reduced linearly independent vectors 
                       -- to iterate through the list and to establish which vectors are linearly independent
                 where base' = extractBase base
                       -- reduced vector (attempt to add the inverse of a linear combination of the other vectors
                       -- representing v to it, making v' = v - v = 0
                       v'  = extendBase base' v

{-
  Input: list of vectors and their corresponding indices (as pairs)
  Output: list of linearly independent vectors and their corresponding indices (as pairs)
  
  Function to filter out vectors that are linearly dependent.
  This is done by extending a partial base using a variant of the Gauss algorithm and filtering out all dependent vectors.
  Dependent vectors will have been transformed in 0 vectors, since all dependent part have been subtracted.
  Basically exactly the same as extractBase, except for keeping track of the indices of the (reduced) vectors.
-}
extractBaseEnum :: (Reducible a) => [(Int, Vector a)] -> [(Int, Vector a)]
extractBaseEnum [] = []
-- filter out linearly dependent vectors recursively
-- (if v' (i.e. vector after attempt to extend the base) is 0-vector it gets ignored (only base' returned)) 
extractBaseEnum ((i,v):base) = if V.all (==0) v' then base' else ((i,v') : base')
                             -- recursively shrinking set of vectors (containing the base to extract) 
                       where base' = extractBaseEnum base
                             -- reducing the vector v with the vectors in base' (second component of the list)
                             v' = extendBase (map snd base') v

{-
  Input: Two vectors to order
  Output: relationship between the vectors (as Ordering)

  Derived the order between the two input vectors (as partial order)
-}
absDescend :: (Ord a, Num a) => Vector a -> Vector a -> Ordering
absDescend v u
  | abs v < abs u = GT
  | abs v > abs u = LT
  | v == u = EQ
