import Data.Map (Map)
import System.Environment (getArgs)
import Text.Regex (mkRegex, matchRegex)
import qualified Data.Map as Map

data Rect = Rect { x :: Int, y :: Int, width :: Int, height :: Int }
            deriving (Show)

allPoints :: Rect -> [Int]
allPoints rect = [ xx + yy * 1000 | xx <- [x rect..x rect + width rect - 1],
                                    yy <- [y rect..y rect + height rect - 1]]

part1 :: [Rect] -> Int
part1 rects = fst $ foldl apply (0, Map.empty) rects
  where apply (count, seen) rect = let newSeen = foldl addPoint seen (allPoints rect)
                                       addPoint map pt = Map.insertWith (+) pt 1 map
                                       delta = length $ filter (\pt -> Map.lookup pt seen == Just 1)
                                                                (allPoints rect)
                                   in (count + delta, newSeen)

parseClaim :: String -> (Int, Rect)
parseClaim line = (id, Rect x y width height)
  where Just [id, x, y, width, height] = map parseInt <$> matchRegex regex line
        regex = mkRegex "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
        parseInt str = read str :: Int

main = do
  [fileName] <- getArgs
  rects <- map (snd . parseClaim) . lines <$> readFile fileName
  print $ part1 rects
  
