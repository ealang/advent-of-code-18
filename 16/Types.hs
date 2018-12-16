module Types (RegisterMap, Sample) where

import Data.Map (Map)

type RegisterMap = Map Int Int
type Sample = (RegisterMap, [Int], RegisterMap) 
