import qualified Data.Set as Set

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 freqs = freqs' Set.empty stream
  where stream = scanl (+) 0 (concat . repeat $ freqs)
        freqs' seen (v:xs) = if Set.member v seen
                             then v
                             else freqs' (Set.insert v seen) xs

main = do
  let parseInt str = read (dropWhile (=='+') str) :: Int
  freqs <- map parseInt . lines <$> readFile "day01.txt"
  print $ part1 freqs
  print $ part2 freqs
