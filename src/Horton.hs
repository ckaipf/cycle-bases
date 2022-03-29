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

horton :: (Reducible a) => Graph -> (Integer -> a) -> [Cycle]
horton g f =
  map (((!!) cs) . fst) $
  extractBaseEnum $
  zip [0 ..] $
  map (V.map f . G.toIncidenceVector . G.subgraph) cs
  where
    cs = reverse . L.sortBy (comparing G.length) . S.toList $ edgeShortCycles g
