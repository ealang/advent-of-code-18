module Day16.Types (RegisterMap, Operation, Instruction, Sample) where

import Data.Map (Map)

type RegisterMap = Map Int Int
type Operation = Int -> Int -> Int -> RegisterMap -> RegisterMap
type Instruction = [Int]
type Sample = (RegisterMap, Instruction, RegisterMap) 
