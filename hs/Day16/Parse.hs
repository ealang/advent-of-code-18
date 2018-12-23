module Day16.Parse (parseSamples, parseInstructions) where

import Data.Map (Map)
import Data.List (partition)
import Text.Regex.Posix ((=~), getAllTextMatches)
import qualified Data.Map as Map

import Day16.Types (RegisterMap, Sample, Instruction)

breakAllOn :: (a -> Bool) -> [a] -> [[a]]
breakAllOn p xs = case dropWhile p xs of
                    []    -> []
                    start -> head:breakAllOn p remain
                      where (head, remain) = break p start

findInts :: String -> [Int]
findInts input = map parseInt (getAllTextMatches $ input =~ "-?[0-9]+" :: [String])
  where parseInt str = read str :: Int

parseSamples :: String -> [Sample]
parseSamples input = map parseCase casesStr
  where casesStr = breakAllOn (=="") (lines input)
        parseCase [before, instr, after] = (parseReg before, findInts instr, parseReg after)
        parseReg str = Map.fromList $ zip [0..] (findInts str)

parseInstructions :: String -> [Instruction]
parseInstructions = map findInts . lines
