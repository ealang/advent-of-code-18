import Control.Monad (forM_)
import Data.Map ((!))
import Data.List (last)
import Data.Vector (Vector)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Instructions (instructionSet)
import Parse (parseInput)
import Types (RegisterMap, Program)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []     = []
takeUntil p (x:xs) = x:if (not . p) x then takeUntil p xs
                                      else []

singleStep :: Int -> Program -> RegisterMap -> RegisterMap
singleStep ip program reg = Map.insertWith (+) ip 1 (instruction reg)
  where instruction = (Vector.!) program (reg ! ip)

executionStream :: Int -> Program -> RegisterMap -> [RegisterMap]
executionStream ip program = takeUntil complete . iterate (singleStep ip program)
  where complete regmap = regmap ! ip >= length program

execute :: Int -> Program -> RegisterMap -> RegisterMap
execute ip program = last . executionStream ip program

renderReg :: RegisterMap -> String
renderReg reg = unwords . map show $ Map.elems reg

part1 ip program = do
  let reg = Map.fromList $ zip [0..5] (repeat 0)
  print $ execute ip program reg ! 0 -- 2072

part2 ip program = do
  let reg = Map.fromList $ zip [0..5] [1, 0, 0, 0, 0, 0]
  forM_ (executionStream ip program reg) (putStrLn . renderReg)

main = do
  (ip, program) <- parseInput "input.txt"
  part1 ip program
  putStrLn "[Hit enter]"
  getLine
  part2 ip program
