import Data.List (find)
import Data.Map ((!), Map)
import qualified Data.Map as Map

import Day22.Pathfinding (bfSearch)

type Pt = (Int, Int)
data Tool = Hands | Torch | ClimbingGear deriving (Ord, Eq)
data RegionType = Rocky | Wet | Narrow

allTools = [Hands, Torch, ClimbingGear]
validMoves = [(-1, 0), (1, 0), (0, -1), (0, 1)]
moveCost = 1
changeCost = 7

toolAllowed :: Tool -> RegionType -> Bool
toolAllowed Hands        Rocky  = False
toolAllowed Torch        Wet    = False
toolAllowed ClimbingGear Narrow = False
toolAllowed _            _      = True

erosionTable :: Int -> Pt -> Pt -> Map Pt Int
erosionTable depth target (w, h) = foldl insert Map.empty coords
  where coords = [ (x, y) | y <- [0..h], x <- [0..w] ]
        insert table pt = Map.insert pt (erosion table pt) table
        erosion table pt = (depth + geoIdx table pt) `mod` 20183
        geoIdx table pt
          | pt == target = 0
          | otherwise    =
             case pt of (x, 0) -> x * 16807
                        (0, y) -> y * 48271
                        (x, y) -> (table ! (x - 1, y)) * (table ! (x, y - 1))

regionTable :: Int -> Pt -> Pt -> Map Pt RegionType
regionTable depth target dim = regionType . (`mod` 3) <$> erosionTable depth target dim
  where regionType 0 = Rocky
        regionType 1 = Wet
        regionType 2 = Narrow

-- Calculate risk level
part1 :: Int -> Pt -> Int
part1 depth target = sum $ fmap risk (regionTable depth target target)
  where risk Rocky  = 0
        risk Wet    = 1
        risk Narrow = 2

-- Find minimum time to reach target
part2 :: Int -> Pt -> Maybe Int
part2 depth target@(tx, ty) = find (Map.member endPt) searchStream >>=
                              Map.lookup endPt
  where startPt = ((0, 0), Torch)
        endPt = (target, Torch)
        searchStream = bfSearch startPt (nextActions regions)
        regions = regionTable depth target (tx + 50, ty + 50)

nextActions :: Map Pt RegionType -> (Pt, Tool) -> [(Int, (Pt, Tool))]
nextActions regions (p@(x, y), t) = filter isValid (moveActions ++ toolActions)
  where moveActions = [ (moveCost, ((x + dx, y + dy), t)) | (dx, dy) <- validMoves ]
        toolActions = [ (changeCost, (p, t')) | t' <- allTools, t' /= t ]
        isValid (_, (p', t')) = (toolAllowed t' <$> Map.lookup p' regions) == Just True

main = do
  let depth = 11739
  let target = (11, 718)
  print $ part1 depth target -- 8735
  print $ part2 depth target -- 984
