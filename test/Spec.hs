{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

import qualified Graph    as G
import qualified Data.List        as L
import           Data.Ratio
import qualified Data.Vector      as V
import           DePina
import qualified EdgeShort        as E
import qualified FloydWarshall    as F
import           GHC.TypeLits
import           Horton
import           Language.Haskell.TH
import           Control.Monad
import           TH

{- TemplateHaskell top-level declarations -}
runQ declareConversionFunctions

conversionFunctions :: [ConversionFunctions]
conversionFunctions = $(conversionFunctionsList primes)

{- TemplateHaskell end -}

main :: IO ()
main = do
  example <- readFile "./test/example3"
  
  let al = V.fromList . map (map typify) . map tupify . map (map readInt) . map words . lines $ example 
      g = G.fromAdjacencyList al
      
      mcb a b f | b == "directed" = L.sort $ a g f
                   | b == "undirected" = L.sort . map (G.mapOverEdgeLabels abs) $ a g (f . abs)
                   | otherwise = error "mcb"
          
      bs = map (\(p,(CF f)) -> (p, mcb dePina "undirected" f)) $ zip primes conversionFunctions

  mapM (\(p,b) -> do
        putStrLn $ "GF(" ++ show p ++ "): " ++ show b
      ) bs

  return ()
  

  
readInt :: String -> Int
readInt = read

typify :: (Int,Int,Int,Int) -> G.Edge
typify (v,u,e,w) = G.Edge v u e (G.Weight (fromIntegral w))

tupify :: [a] -> [(a,a,a,a)]
tupify []           = []
tupify (p:x:y:z:xs) = (p,x,y,z) : tupify xs
