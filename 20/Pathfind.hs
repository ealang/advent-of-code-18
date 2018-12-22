module Pathfind (bfsDistances) where

import Data.Sequence ((><), Seq, ViewL(..))
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Types (Point)

bfsDistances :: (Point -> [Point]) -> (Int, Int) -> [Int]
bfsDistances neighbors startPt = bfs (Seq.singleton (0, startPt)) Set.empty
  where nbrsWithDist pt dist = Seq.fromList (zip (repeat (dist + 1)) (neighbors pt))
        bfs toVisit visited =
          case Seq.viewl toVisit of
            EmptyL             -> []
            ((dist, pt) :< xs) -> if pt `Set.member` visited
                                  then bfs xs visited
                                  else dist : bfs (xs >< nbrsWithDist pt dist)
                                                  (pt `Set.insert` visited)
