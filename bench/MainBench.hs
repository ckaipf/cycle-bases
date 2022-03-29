{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Graph    as G
import           Data.FiniteField
import qualified Data.List        as L
import           Data.Ratio
import qualified Data.Vector      as V
import           DePina
import           GHC.TypeLits
import           Horton
import qualified Data.Set as S
import Criterion.Main
import EdgeShort
import FloydWarshall

directedHortonF2 g  = L.sort $ horton g f2
directedDePinaF2 g = L.sort $ dePina g f2
directedHortonZ g  = L.sort $ horton g z
directedDePinaQ g = L.sort $ dePina g q


z a = (fromInteger a) :: (Integer)
q a = (fromInteger a) :: (Ratio Integer)
f2 a = (fromInteger a) :: (PrimeField 2)



typify :: (Int,Int,Int,Int) -> G.Edge
typify (v,u,e,w) = G.Edge v u e (G.Weight (fromIntegral w))

diamond :: Int -> [(Int,Int,Int,Int)]
diamond n = f 0 1 []
  where f v i acc | v == n          = (v,0,i,1):acc
                  | mod v 4 == 1    = f (v+1) (i+1) ((v,v+2,i,1):acc)
                  | mod v 4 == 0    = f (v+1) (i+2) ((v,v+1,i,1):(v,v+2,i+1,1):acc)
              --  | mod v 4 == 2    = f (v+1) (i+1) ((v,v+1,i,1):acc)
              --  | mod v 4 == 3    = f (v+1) (i+1) ((v,v+1,i,1):acc)
                  | otherwise = f (v+1) (i+1) ((v,v+1,i,1):acc)


ns = [(n*4)-1 | n <- [1..10]]
f n = (n,g)
  where es = S.fromList $ map typify $ diamond n
        vs = G.edgesToVertices es
        g = G.Graph es vs

f1 n = (n,g')
  where es = S.fromList $ map typify $ diamond n
        es' = S.map G.inverse es
        g' = G.Graph (S.union es es') (G.edgesToVertices es)

edgy g = edgeShort (S.toList $ G.edges g) (V.fromList $ shortestPaths g)

main :: IO ()
main = let gs = map f ns
           gs' = map f1 ns

       in defaultMain [
  bgroup "floydWarshall" $ map (\(n,g) -> bench (show n) $ whnf floydWarshall g) gs',
  bgroup "shortestPaths" $ map (\(n,g) -> bench (show n) $ whnf shortestPaths g) gs',
  bgroup "edgeShort" $ map (\(n,g) -> bench (show n) $ whnf edgy g) gs',
  bgroup "edgeShortCycles" $ map (\(n,g) -> bench (show n) $ whnf edgeShortCycles g) gs,
  bgroup "hortonF2" $ map (\(n,g) -> bench (show n) $ whnf directedHortonF2 g) gs
 -- bgroup "dePinaF2" $ map (\(n,g) -> bench (show n) $ whnf directedDePinaF2 g) gs
  ]


