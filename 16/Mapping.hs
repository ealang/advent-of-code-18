module Mapping (findMapping) where

import Data.List ((!!))
import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.List as List

findCandidateMapping :: (a -> b -> Bool) -> Map Int [a] -> Map Int b -> Map Int [Int]
findCandidateMapping canExplain observations explanations = Map.fromList [ (i, matches i) | i <- [0..length explanations - 1] ]
  where matches i = let samples = observations ! i
                        matchingExpl = filter (passesAll samples) (Map.toList explanations)
                    in fmap fst matchingExpl
        passesAll samples (_, op) = all (`canExplain` op) samples

reduceMapping :: Map Int [Int] -> [(Int, Int)]
reduceMapping candidates = case findSingle of
                            Just (i, [j]) -> (i, j):reduceMapping (without i j)
                            _             -> []
  where findSingle = List.find (\(i, elems) -> length elems == 1) (Map.toList candidates)
        without i j = Map.delete i (fmap (filter (/=j)) candidates)

-- Given keyed observations and a list of explanations, use canExplain to
-- match an explanation to each type of observation.
findMapping :: (a -> b -> Bool) -> Map Int [a] -> [b] -> Map Int b
findMapping canExplain observations explanations =
  fmap (\i -> explanations !! i) .
  Map.fromList .
  reduceMapping $
    findCandidateMapping canExplain observations (Map.fromList $ zip [0..] explanations)
