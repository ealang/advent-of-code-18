import Data.List (sortBy, groupBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Vector (Vector, (!))
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Day16.Parse (parseSamples, parseInstructions)
import Day16.Types (RegisterMap, Operation, Instruction, Sample)
import Day16.Instructions (instructionSet)
import Day16.Mapping (findMapping)

execInstruction :: Operation -> RegisterMap -> Instruction -> RegisterMap
execInstruction op before [_, p1, p2, p3] =
  op p1 p2 p3 before

testInstruction :: Sample -> Operation -> Bool
testInstruction (before, instr, after) op =
  execInstruction op before instr == after

groupSamplesByOpcode :: [Sample] -> Map Int [Sample]
groupSamplesByOpcode samples = Map.fromList $ zip [0..] (groupBy sameOpcode (sortBy (comparing opcode) samples))
  where opcode (_, op:_, _) = op
        sameOpcode s1 s2 = opcode s1 == opcode s2

-- Count number of samples that had 3 or more matching instructions
part1 :: [Sample] -> [Operation] -> Int
part1 samples instructions = length . filter (>=3) $ perSampleMatches 
  where perSampleMatches = map (length . filter (==True) . testSample) samples
        testSample sample = map (testInstruction sample) instructions

-- Find register contents after executing the given program
part2 :: Vector Operation -> RegisterMap -> [Instruction] -> RegisterMap
part2 mapping = foldl exec
  where exec regMap' instr@(op:_) = execInstruction (mapping ! op) regMap' instr

main = do
  samples <- parseSamples <$> readFile "Day16/input.txt"
  instructions <- parseInstructions <$> readFile "Day16/testprog.txt"

  print $ part1 samples instructionSet -- 570

  let groupedSamples = groupSamplesByOpcode samples
  let mapping = Vector.fromList . Map.elems $ findMapping testInstruction groupedSamples instructionSet
  let initReg = Map.fromList $ zip [0..3] (repeat 0)
  print $ part2 mapping initReg instructions -- 503

