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

class (Num a, Ord a) =>
      Reducible a where
  reduceVector :: Vector a -> Vector a -> Vector a

instance (KnownNat p) => Reducible (PrimeField p) where
  reduceVector u v = u + k `scale` v
    where
      i =
        case (V.findIndex (/= 0) v) of
          Nothing -> error "zero vector"
          Just x -> x
      k = (-(u ! i)) * (recip (v ! i))

instance Reducible Integer where
  reduceVector u v
    | u ! i == 0 = u
    | otherwise =
      (m `div` (abs u ! i)) `scale` u + (s * (m `div` (abs v ! i))) `scale` v
    where
      i =
        case (V.findIndex (/= 0) v) of
          Nothing -> error "zero vector"
          Just x -> x
      m = lcm (u ! i) (v ! i)
      s =
        if signum (u ! i) == signum (v ! i)
          then (-1)
          else 1

instance (Num a) => Num (Vector a) where
  x + y = V.zipWith (+) x y
  x - y = V.zipWith (-) x y
  abs x = V.map (abs) x

scale :: Num a => a -> Vector a -> Vector a
scale e v = V.map (*e) v

-- takes a vector and a base and returns reduced vector
extendBase :: (Reducible a) => [Vector a] -> Vector a -> Vector a
extendBase base v  = foldl' (reduceVector) v (sortBy absDescend base)

-- takes a set of vectors (with unknown dependence) and returns a subset of linear independent vectors
extractBase :: (Reducible a) => [Vector a] -> [Vector a]
extractBase [] = []
extractBase (v:base) = if V.all (==0) v' then base' else v' : base'
                 where base' = extractBase base
                       v'  = extendBase base' v

extractBaseEnum :: (Reducible a) => [(Int, Vector a)] -> [(Int, Vector a)]
extractBaseEnum [] = []
extractBaseEnum ((i,v):base) = if V.all (==0) v' then base' else ((i,v') : base')
                       where base' = extractBaseEnum base
                             v' = extendBase (map snd base') v

absDescend :: (Ord a, Num a) => Vector a -> Vector a -> Ordering
absDescend v u
  | abs v < abs u = GT
  | abs v > abs u = LT
  | v == u = EQ
