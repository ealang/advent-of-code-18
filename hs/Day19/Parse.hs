module Day19.Parse (parseInput, renderReg) where 

import Data.Map ((!))
import Data.Vector (Vector)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Day19.Instructions (instructionSet)
import Day19.Types (RegisterMap, BoundInstruction, Program)

parseInput :: String -> IO (Int, Program)
parseInput fileName = do
  ipLine:insLines <- lines <$> readFile fileName
  let _:[ip] = words ipLine
  let instr = map parseInstr insLines
  return (readInt ip, Vector.fromList instr)

readInt :: String -> Int
readInt s = read s :: Int

parseInstr :: String -> BoundInstruction
parseInstr line = (instructionSet ! instr) (readInt op1) (readInt op2) (readInt op3)
  where [instr, op1, op2, op3] = words line

renderReg :: RegisterMap -> String
renderReg reg = unwords . map show $ Map.elems reg
