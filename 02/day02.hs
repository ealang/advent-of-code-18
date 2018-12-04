import Data.Map (Map)
import Data.Tuple (swap)
import Control.Monad (msum)
import qualified Data.Map as Map

-- Compute frequency of elements in a list
freqMap :: Ord a => [a] -> Map Int a
freqMap list = reverseMap $ foldl insert Map.empty list
  where insert freqs elem = Map.insertWith (+) elem 1 freqs
        reverseMap m = Map.fromList . map swap $ Map.toList m

-- Calculate checksum of ids
part1 :: [String] -> Int
part1 ids = doubleCount * tripleCount
  where doubleCount = length . filter hasDouble $ ids
        tripleCount = length . filter hasTriple $ ids
        hasDouble id = Map.member 2 $ freqMap id
        hasTriple id = Map.member 3 $ freqMap id

-- Drop ith char from a string
dropChar :: Int -> String -> String
dropChar i string = first ++ second
  where (first, _:second) = splitAt i string

-- Find common part of two ids that differ by a single character
part2 :: [String] -> Int -> Maybe String
part2 ids codeLength = msum allIndexes
  where allIndexes = [ findDup $ map (dropChar i) ids
                        | i <- [0..codeLength - 1] ]
        findDup ids' = Map.lookup 2 $ freqMap ids'

main = do
  ids <- lines <$> readFile "day02.txt"
  print $ part1 ids
  let codeLength = length $ head ids
  print $ part2 ids codeLength

