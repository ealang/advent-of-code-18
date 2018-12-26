module Day22.Pathfinding (bfSearch) where

import Data.Map (Map)
import Control.Arrow (first)
import qualified Data.Map as Map
import qualified Data.Set as Set

bfSearch :: (Ord a) => a -> (a -> [(Int, a)]) -> [Map a Int]
bfSearch start neighbors = next Map.empty (Set.singleton (0, start))
  where newNodes pt cost = Set.fromList $ fmap (first (cost +)) (neighbors pt)
        next seen toVisit
          | null toVisit = []
          | otherwise =
            case Set.deleteFindMin toVisit of
              ((cost, pt), toVisit') ->
                if pt `Map.notMember` seen
                  then seen : next (Map.insert pt cost seen)
                                   (Set.union toVisit' (newNodes pt cost))
                  else next seen toVisit'
