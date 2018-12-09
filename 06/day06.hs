import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int)
type BBox = (Point, Point)

pointsBy :: Point -> [Point]
pointsBy (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

boundingBox :: [Point] -> BBox
boundingBox pts = ((minimum xVals, minimum yVals), (maximum xVals, maximum yVals))
  where xVals = map fst pts
        yVals = map snd pts

boundingBoxContains :: Point -> BBox -> Bool
boundingBoxContains (x, y) ((tx, ty), (bx, by)) = x >= tx && y >= ty && x <= bx && y <= by

mdist :: Point -> Point -> Int
mdist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- Breadth first search from a point
walkBfs :: (Point -> Bool) -> Point -> [Point]
walkBfs canMoveTo start = concatMap snd .
                            takeWhile (\(_, pts) -> not (null pts)) $
                            iterate walk (Set.singleton start, [start])
  where walk (seen, toVisit) = foldl update (seen, []) toVisit
        update cur@(seen, toVisit) pt =
          (Set.union seen (Set.fromList ptsAround), ptsAround ++ toVisit)
            where ptsAround = filter (\pt -> not (Set.member pt seen) && canMoveTo pt) $ pointsBy pt

-- Iterate all points closest to a given point
traverseTerritory :: Point -> [Point] -> [Point]
traverseTerritory point points = walkBfs canMoveTo point
  where canMoveTo point' = nearestPointTo point' == point
        nearestPointTo point' = minimumBy (compare point')  points
        compare point' p1 p2
          | d1 < d2 = LT
          | d1 > d2 = GT
          | p1 == point = GT
          | p2 == point = LT
          | otherwise = EQ
          where d1 = mdist point' p1
                d2 = mdist point' p2

-- Find size of most isolated region
part1 :: [Point] -> Maybe Int
part1 points = maximum [territory pt | pt <- points]
  where territory pt = countBounded 0 $ traverseTerritory pt points
        bbox = boundingBox points
        countBounded tot []      = Just tot
        countBounded tot (pt:xs) = if boundingBoxContains pt bbox
                                   then countBounded (tot + 1) xs
                                   else Nothing

-- Find size of region where all points are within the given limit
part2 :: Int -> [Point] -> Int
part2 limit points = length $ walkBfs canMoveTo (x, y)
  where canMoveTo pt = sum [mdist pt pt' | pt' <- points] <= limit
        ((minX, minY), (maxX, maxY)) = boundingBox points
        xvals = map fst points
        yvals = map snd points
        x = snd $ minimum [(sum [abs (i - x') | x' <- xvals], i) | i <- [minX..maxX]]
        y = snd $ minimum [(sum [abs (i - y') | y' <- yvals], i) | i <- [minY..maxY]]

parsePt :: String -> Point
parsePt str = (parseInt x, parseInt y)
   where [x, y]     = splitOn ", " str
         parseInt s = read s :: Int

main = do
  points <- map parsePt . lines <$> readFile "day06.txt"
  print $ part1 points
  print $ part2 10000 points 
