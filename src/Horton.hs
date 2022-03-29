module Horton
  ( horton
  ) where

import qualified Data.List   as L
import           Data.Ord
import qualified Data.Set    as S
import qualified Data.Vector as V
import           EdgeShort
import           Gauss
import           Graph       (Cycle, Graph)
import qualified Graph       as G

{- 
  Implementation of the Horton algorithm
  Input: Graph g to extract the MCB from and field map f
  Output: MCB as a cycle set
  
  Function that extracts the first linearly independent cycles from a candidate set of edgeShortCycles
-}
horton :: (Reducible a) => Graph -> (Integer -> a) -> [Cycle]
horton g f =
  -- return the respective cycles based on the indices of the linearly independent cycles (that horton yields via extractBaseEnum)
  map (((!!) cs) . fst) $
  -- extract a linearly independent subset from the indexed candidate set
  extractBaseEnum $
  -- indexing the transformed candidate set
  zip [0 ..] $
  -- candidate set is transformed via the field map (as incidence vectors)
  map (V.map f . G.toIncidenceVector . G.subgraph) cs
  where
    -- cycle candidate set derived by sorting the set of edgeShortCycles (EdgeShort.hs) in ascending length
    cs = reverse . L.sortBy (comparing G.length) . S.toList $ edgeShortCycles g
