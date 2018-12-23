module Day18.Settlers (Terrain, renderTerrain, statesFrom, resourceValue) where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Vector ((!), Vector)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

type Terrain = Vector Char

tree = '|'
open = '.'
lumberYard = '#'

countFreq :: Eq a => a -> [a] -> Int
countFreq item list = length $ filter (==item) list

neighborsOf :: Int -> Int -> [Int]
neighborsOf n i = map toIndex . filter inBounds $ map (toXY i) adjacent
  where toXY i (dx, dy) = (i `mod` n + dx, i `div` n + dy)
        toIndex (x, y) = x + y * n
        inBounds (x, y) = x >= 0 && y >= 0 && x < n && y < n
        adjacent = [(-1, 0), (1, 0), (0, -1), (0, 1),
                    (-1, -1), (-1, 1), (1, -1), (1, 1)]

withNeighbors :: Int -> Terrain -> Vector (Char, String)
withNeighbors n terrain = fmap getNbrs (Vector.zip (Vector.fromList [0..n * n - 1]) terrain)
  where getNbrs (i, cell) = (cell, [terrain ! j | j <- neighborsOf n i ])

nextCellVal :: Char -> String -> Char
nextCellVal cell nbrs
  | cell == open       && ntrees >= 3                = tree
  | cell == tree       && nyards >= 3                = lumberYard
  | cell == lumberYard && (nyards == 0 || ntrees == 0) = open
  | otherwise                                        = cell
  where ntrees = countFreq tree nbrs
        nyards = countFreq lumberYard nbrs

nextState :: Int -> Terrain -> Terrain
nextState n terrain = fmap (uncurry nextCellVal) (withNeighbors n terrain)

statesFrom :: Int -> Terrain -> [Terrain]
statesFrom n = iterate (nextState n)

countResources :: Terrain -> Map Char Int
countResources = foldl insert Map.empty
  where insert totals cell = Map.insertWith (+) cell 1 totals

resourceValue :: Terrain -> Int
resourceValue terrain = ntrees * nyards
  where ntrees = Map.findWithDefault 0 tree resources
        nyards = Map.findWithDefault 0 lumberYard resources
        resources = countResources terrain

renderTerrain :: Int -> Terrain -> String
renderTerrain n terrain = intercalate "\n" (chunksOf n (Vector.toList terrain))
