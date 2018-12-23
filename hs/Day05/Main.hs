import Data.Char (toLower)

-- Determine if two units react
reacts :: Char -> Char -> Bool
reacts a b = a /= b && toLower a == toLower b

-- Reduce a polymer fully
reduce :: String -> String
reduce []     = []
reduce (a:xs) = case reduce xs of
                  []      -> [a]
                  (b:xs') -> if reacts a b
                             then xs'
                             else a:b:xs'

-- Find length of fully reduced polymer
part1 :: String -> Int
part1 = length . reduce

-- Find the minimum length polymer that can by obtained
-- by removing all units of a certain type
part2 :: String -> Int
part2 polymer = minimum [score unit | unit <- ['a'..'z']]
  where score unit = part1 $
                     filter (not . isUnit unit) polymer
        isUnit unit c = toLower c == unit

main = do
  polymer <- takeWhile (/='\n') <$> readFile "Day05/input.txt"
  print $ part1 polymer
  print $ part2 polymer
