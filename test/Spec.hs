{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Graph    as G
import           Data.FiniteField
import qualified Data.List        as L
import           Data.Ratio
import qualified Data.Vector      as V
import           DePina
import qualified EdgeShort        as E
import qualified FloydWarshall    as F
import           GHC.TypeLits
import           Horton
import qualified Data.Set as S

main :: IO ()
main = do
  example <- readFile "./test/example3"
  let al =
        V.fromList .
        map (map typify) . map tupify . map (map readInt) . map words . lines $
        example
      g = G.fromAdjacencyList al

      directedHortonOver f   = L.sort $ horton g f
      directedDePinaOver f   = L.sort $ dePina g f
      undirectedHortonOver f = L.sort . map (G.mapOverEdgeLabels abs) $ horton g (f . abs)
      undirectedDePinaOver f = L.sort $ dePina g (f . abs)

      z a = (fromInteger a) :: (Integer)
      q a = (fromInteger a) :: (Ratio Integer)
      f2 a = (fromInteger a) :: (PrimeField 2)
      f3 a = (fromInteger a) :: (PrimeField 3)
      f5 a = (fromInteger a) :: (PrimeField 5)
      f7 a = (fromInteger a) :: (PrimeField 7)
      f11 a = (fromInteger a) :: (PrimeField 11)
      f13 a = (fromInteger a) :: (PrimeField 13)
      f17 a = (fromInteger a) :: (PrimeField 17)
      f19 a = (fromInteger a) :: (PrimeField 19)

  putStrLn "\n directed  Z / Q"
  print $  directedHortonOver z
  print $  directedDePinaOver q
  putStrLn "\n undirected  Z / Q"
  print $  undirectedHortonOver z
  print $  undirectedDePinaOver q

  putStrLn "\n Horton/DePina from F2 to F13"
  print $  undirectedHortonOver f2
  print $  undirectedDePinaOver f2
  print $  undirectedHortonOver f3
  print $  undirectedDePinaOver f3
  print $  undirectedHortonOver f5
  print $  undirectedDePinaOver f5
  print $  undirectedHortonOver f7
  print $  undirectedDePinaOver f7
  print $  undirectedHortonOver f11
  print $  undirectedDePinaOver f11
  print $  undirectedHortonOver f13
  print $  undirectedDePinaOver f13

  let es = S.fromList $ map typify $ diamond 3
      vs = G.edgesToVertices es
      g1 = G.Graph es vs
  print $ L.sort $ dePina g1 (f2)

readInt :: String -> Int
readInt = read

typify :: (Int,Int,Int,Int) -> G.Edge
typify (v,u,e,w) = G.Edge v u e (G.Weight (fromIntegral w))

tupify :: [a] -> [(a,a,a,a)]
tupify []           = []
tupify (p:x:y:z:xs) = (p,x,y,z) : tupify xs

diamond :: Int -> [(Int,Int,Int,Int)]
diamond n = f 0 1 []
  where f v i acc | v == n                           = (v,0,i,1):acc
                  | mod v 4 == 1                     = f (v+1) (i+1) ((v,v+2,i,1):acc)
                  | mod v 4 == 2                     = f (v+1) (i+1) ((v,v+1,i,1):acc)
                  | mod v 4 == 3                     = f (v+1) (i+1) ((v,v+1,i,1):acc)
                  | mod v 4 == 0                     = f (v+1) (i+2) ((v,v+1,i,1):(v,v+2,i+1,1):acc)
                  | otherwise = acc
