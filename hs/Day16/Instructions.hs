module Day16.Instructions (instructionSet) where

import Data.Bits ((.&.), (.|.))
import Data.Map ((!))
import qualified Data.Map as Map

import Day16.Types (RegisterMap, Operation)

instructionSet :: [Operation]
instructionSet = [ instrAddr,
                   instrAddi,
                   instrMulr,
                   instrMuli,
                   instrBanr,
                   instrBani,
                   instrBorr,
                   instrBori,
                   instrSetr,
                   instrSeti,
                   instrGtir,
                   instrGtri,
                   instrGtrr,
                   instrEqir,
                   instrEqri,
                   instrEqrr ]

regOp :: (Int -> Int -> Int) -> Int -> Int -> Int -> RegisterMap -> RegisterMap
regOp op ra rb rc registers = Map.insert rc vc registers
  where vc = op (registers ! ra) (registers ! rb)

valOpRV :: (Int -> Int -> Int) -> Int -> Int -> Int -> RegisterMap -> RegisterMap
valOpRV op ra vb rc registers = Map.insert rc vc registers
  where vc = op (registers ! ra) vb

valOpVR :: (Int -> Int -> Int) -> Int -> Int -> Int -> RegisterMap -> RegisterMap
valOpVR op va rb = valOpRV (flip op) rb va

greaterThan a b = if a > b then 1 else 0
equalTo a b = if a == b then 1 else 0

-- addr (add register) stores into register C the result of adding register A and register B.
instrAddr = regOp (+)

-- addi (add immediate) stores into register C the result of adding register A and value B.
instrAddi = valOpRV (+)

-- mulr (multiply register) stores into register C the result of multiplying register A and register B.
instrMulr = regOp (*)

-- muli (multiply immediate) stores into register C the result of multiplying register A and value B.
instrMuli = valOpRV (*)

-- banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
instrBanr = regOp (.&.)

-- bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
instrBani = valOpRV (.&.)

-- borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
instrBorr = regOp (.|.)

-- bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
instrBori = valOpRV (.|.)

-- setr (set register) copies the contents of register A into register C. (Input B is ignored.)
instrSetr = regOp const

-- seti (set immediate) stores value A into register C. (Input B is ignored.)
instrSeti = valOpVR const

-- gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
instrGtir = valOpVR greaterThan

-- gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
instrGtri = valOpRV greaterThan

-- gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
instrGtrr = regOp greaterThan

-- eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
instrEqir = valOpVR equalTo

-- eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
instrEqri = valOpRV equalTo

-- eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
instrEqrr = regOp equalTo
