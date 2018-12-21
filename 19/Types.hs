module Types (RegisterMap, Instruction, BoundInstruction, Program) where

import Data.Map (Map)
import Data.Vector (Vector)

type RegisterMap = Map Int Int
type Instruction = Int -> Int -> Int -> RegisterMap -> RegisterMap
type BoundInstruction = RegisterMap -> RegisterMap
type Program = Vector BoundInstruction
