module Day19.Computer (executionStream, execute) where

import Data.Map ((!))
import Data.List (last)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Day19.Types (RegisterMap, Program)

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
