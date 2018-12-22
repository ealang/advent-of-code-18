import Control.Arrow ((&&&))
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Parse (parseInput)
import Pathfind (bfsDistances)
import Types (Direction(..), Element(..), Point)

cellEmpty = ' '
cellDoor = '*'
cellWall = '#'

walk :: Point -> Direction -> Point
walk (x, y) N = (x, y + 1)
walk (x, y) E = (x + 1, y)
walk (x, y) S = (x, y - 1)
walk (x, y) W = (x - 1, y)

walkPath :: Point -> [Direction] -> [Point]
walkPath = scanl walk 

walkDest :: Point -> [Direction] -> Point
walkDest pt = last . walkPath pt

-- Given an element, generate all (point, directions) fragments
-- that should be explored in order to explore the full map.
genFragments :: Point -> Element -> IO [(Point, [Direction])]
genFragments start frags = do
  cache <- newIORef Set.empty
  let gen _  []                = return []
      gen pt elems@(elem : xs) = do
        seen <- readIORef cache
        let key = (pt, elems)
        if key `Set.member` seen
          then return []
          else modifyIORef cache (Set.insert key) >> case elem of
            Directions dirs   -> (:) (pt, dirs) <$> gen (walkDest pt dirs) xs
            Sequence children -> gen pt (children ++ xs)
            Branch children   -> concat <$> mapM (\c -> gen pt (c : xs)) children
  gen start [frags]
  
-- Use fragments to build a map of the facility
buildMap :: [(Point, [Direction])] -> Map Point Char
buildMap frags = foldl writeStep Map.empty (concatMap flatten frags)
  where flatten (pt, dirs) = zip (walkPath pt dirs) dirs
        writeStep cur (pt, dir) = foldl writeCell cur [(m2 pt, cellEmpty),
                                                       (walk (m2 pt) dir, cellDoor),
                                                       (m2 (walk pt dir), cellEmpty)]
        writeCell cur (pt, c) = Map.insert pt c cur
        m2 (x, y) = (x * 2, y * 2)

renderMap :: Map Point Char -> String
renderMap cells = unlines [ map (char y) [xmin..xmax] | y <- [ymax, ymax - 1..ymin]]
  where xvals = map fst $ Map.keys cells
        yvals = map snd $ Map.keys cells
        (xmin, xmax) = (minimum &&& maximum) xvals
        (ymin, ymax) = (minimum &&& maximum) yvals
        char y x = Map.findWithDefault cellWall (x, y) cells

neighbors :: Map Point Char -> Point -> [Point]
neighbors cells pt = [walk pt dir | dir <- [N, E, S, W], canWalk pt dir]
  where canWalk pt dir = Map.findWithDefault cellWall (walk (m2 pt) dir) cells == cellDoor
        m2 (x, y) = (x * 2, y * 2)

main = do 
  elements <- parseInput .
              tail . init .
              takeWhile (/='\n') <$> readFile "input.txt"

  let startPt = (0, 0)
  cells <- buildMap <$> genFragments startPt elements
  let roomDistances = bfsDistances (neighbors cells) startPt

  putStrLn $ renderMap cells
  print $ maximum roomDistances -- 3755
  print $ length (filter (>=1000) roomDistances) -- 8627
