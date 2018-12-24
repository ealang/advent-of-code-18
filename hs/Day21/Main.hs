import Data.Map ((!))
import Data.Vector ((//))
import qualified Data.Set as Set
import qualified Data.Map as Map

import Day19.Computer (executionStream)
import Day19.Parse (parseInput)
import Day19.Types (BoundInstruction)

fastLoop :: BoundInstruction
fastLoop reg = Map.insert 4 counter reg
  where target = reg ! 1
        counter = target `div` 256

takeUnique :: Ord a => [a] -> [a]
takeUnique list = map fst .
                  takeWhile (not . uncurry Set.member) $
                    zip list (withHist list)
  where withHist = scanl (flip Set.insert) Set.empty

main = do
  (ip, program) <- parseInput "Day21/input.txt"

  -- Hack program to avoid long loop
  let fastProgram = program // [(17, fastLoop)]

  -- Find values reg 0 can be in order to terminate
  let reg = Map.fromList $ zip [0..5] (repeat 0)
  let values = takeUnique .
               map (! 3) .
               filter (\reg -> reg ! ip == 29) $
                 executionStream ip fastProgram reg

  putStrLn $ "Part1: " ++ show (head values)
  putStrLn $ "Part2: " ++ show (last values)
