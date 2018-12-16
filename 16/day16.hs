import Data.Map (Map, (!))
import qualified Data.Map as Map

import Parse (parseSamples)
import Types (RegisterMap, Sample)
import Instructions (instructionSet)

testInstructions :: Sample -> [Bool]
testInstructions (before, [_, op1, op2, op3], after) = map (==after) results
  where results = map (\i -> i op1 op2 op3 before) instructionSet

part1 :: [Sample] -> Int
part1 samples = length $ filter (>=3) (matches samples)
  where matches = map (length . filter (==True) . testInstructions)

main = do
  samples <- parseSamples <$> readFile "day16-input.txt"
  print $ part1 samples -- 570
