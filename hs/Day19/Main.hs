import Control.Monad (forM_)
import Data.Map ((!))
import qualified Data.Map as Map

import Day19.Parse
import Day19.Computer

part1 ip program = do
  let reg = Map.fromList $ zip [0..5] (repeat 0)
  print $ execute ip program reg ! 0 -- 2072

part2 ip program = do
  -- Figure out the pattern by watching and modifying register values
  let reg = Map.fromList $ zip [0..5] [1, 0, 0, 0, 0, 0]
  forM_ (executionStream ip program reg) (putStrLn . renderReg)

main = do
  (ip, program) <- parseInput "Day19/input.txt"
  part1 ip program

  -- adhoc part2
  print $ sum [i | i <- [1..10551276], 10551276 `rem` i == 0]

  putStrLn "[Hit enter]"
  getLine
  part2 ip program
