import Data.List (sort, maximumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust, isNothing)
import Data.Map ((!), Map)
import Text.Regex (mkRegex, matchRegex)
import qualified Data.Map as Map

data Observation = Asleep | WakeUp | Begin Int deriving (Show, Ord, Eq)

-- Split list on "key frame" elements, returning [(key, values)]
splitOnKeyFrames :: (a -> Maybe b) -> [a] -> [(b, [a])]
splitOnKeyFrames extract elems =
  case dropWhile (isNothing . extract) elems of
    []             -> []
    (keyElem:tail) -> (key, segment):splitOnKeyFrames extract elems'
      where Just key = extract keyElem
            (segment, elems') = break (isJust . extract) tail

-- Split list on "key frame" elements, grouping the values for each key
groupByKeyFrames :: Ord b => (a -> Maybe b) -> [a] -> Map b [a]
groupByKeyFrames extract elems = Map.fromListWith (++) (splitOnKeyFrames extract elems)

-- Compute total time asleep given a timeline
sleepTime :: [(Int, Observation)] -> Int
sleepTime notes = fst $ foldl next (0, Nothing) notes
  where next (total, Just start) (end,   WakeUp) = (total + end - start, Nothing)
        next (total, Nothing   ) (start, Asleep) = (total, Just start)

-- Find time when asleep most often, returning (count, minute)
mostAsleepMinute :: [(Int, Observation)] -> (Int, Int)
mostAsleepMinute notes = snd $ foldl next (0, (0, 0)) (sort notes)
  where next (count, best                ) (_   , WakeUp) = (count - 1, best)
        next (count, best@(bcount, btime)) (time, Asleep) = (count + 1, best')
          where best' = if count + 1 > bcount
                        then (count + 1, time) else best

-- Find guard that spent the most time sleeping.
-- Return guard id * minute where asleep most often.
part1 :: Map Int [(Int, Observation)] -> Int
part1 notes = guardId * bestMinute
  where guardScores = fmap sleepTime notes
        (guardId, _) = maximumBy (comparing snd) (Map.toList guardScores)
        bestMinute = snd $ mostAsleepMinute (notes ! guardId)
        
-- Find guard with most consistent sleep schedule.
-- Return guard id * minute where asleep most often.
part2 :: Map Int [(Int, Observation)] -> Int
part2 notes = guardId * bestMinute
  where guardScores = fmap (fst . mostAsleepMinute . sort) notes
        (guardId, _) = maximumBy (comparing snd) (Map.toList guardScores)
        bestMinute = snd $ mostAsleepMinute (notes ! guardId)

parseLine :: String -> (Int, Observation)
parseLine line = (parseInt minute, note)
  where splitRe = mkRegex "\\[[0-9]+-[0-9]+-[0-9]+ [0-9]+:([0-9]+)\\] (.*)" 
        beginRe = mkRegex "Guard #([0-9]+) begins shift"
        parseInt str = read str :: Int
        Just [minute, noteStr] = matchRegex splitRe line
        note = case noteStr of
                 "falls asleep" -> Asleep
                 "wakes up" -> WakeUp
                 _ -> Begin (parseInt id)
                        where Just [id] = matchRegex beginRe noteStr

main = do
  let keyFunc (_, Begin id) = Just id
      keyFunc _             = Nothing
  notes <- groupByKeyFrames keyFunc . map parseLine . sort . lines <$> readFile "Day04/input.txt"
  print $ part1 notes
  print $ part2 notes
