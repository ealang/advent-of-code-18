import Data.List ((!!), find)
import Data.Map (Map)
import System.Environment (getArgs)
import Text.Regex (mkRegex, matchRegex)
import qualified Data.Map as Map

data Rect = Rect { x :: Int, y :: Int, width :: Int, height :: Int }
            deriving (Show)

allPoints :: Rect -> [Int]
allPoints rect = [ xx + yy * 1000 | xx <- [x rect..x rect + width rect - 1],
                                    yy <- [y rect..y rect + height rect - 1]]

hitMap :: [Rect] -> Map Int Int
hitMap = foldl apply Map.empty
  where apply seen rect = foldl addPoint seen (allPoints rect)
        addPoint map pt = Map.insertWith (+) pt 1 map

-- Count square inches with overlap
part1 :: [Rect] -> Int
part1 = length . filter (>1) . Map.elems . hitMap

-- Identify a rectangle with no overlap
part2 :: [Rect] -> Maybe Int
part2 rects = fst <$> find (hasSingleOverlap (hitMap rects)) (zip [0..] rects)
  where hasSingleOverlap hits (_, rect) = all (\pt -> Map.lookup pt hits == Just 1)
                                              (allPoints rect)

parseClaim :: String -> (Int, Rect)
parseClaim line = (id, Rect x y width height)
  where Just [id, x, y, width, height] = map parseInt <$> matchRegex regex line
        regex = mkRegex "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
        parseInt str = read str :: Int

main = do
  [fileName] <- getArgs
  claims <- map parseClaim . lines <$> readFile fileName
  let rects = map snd claims
  print $ part1 rects
  print $ (!!) claims <$> part2 rects
  
