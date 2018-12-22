module Parse (parseInput) where

import Data.List.Split (splitWhen, chop)
import Types (Direction(..), Element(..))

parseInput :: String -> Element
parseInput str = Sequence (chop chopElem str)

parseDirection :: Char -> Direction
parseDirection 'N' = N
parseDirection 'E' = E
parseDirection 'S' = S
parseDirection 'W' = W

chopElem :: String -> (Element, String)
chopElem str
  | head str == lparen = (parseAsBranch (takeBranch str), dropBranch str)
  | otherwise          = (parseAsDirs (takeWhile (/=lparen) str), dropWhile (/=lparen) str)
  where lparen = '('
        takeBranch str = tail . stripDepth $ takeWhile ((>0) . fst) (withBranchDepth str)
        dropBranch str = tail . stripDepth $ dropWhile ((>0) . fst) (withBranchDepth str)

parseAsDirs :: String -> Element
parseAsDirs str = Directions (map parseDirection str)

parseAsBranch :: String -> Element
parseAsBranch str = Branch branches
  where branches = map parseInput branchStrs
        branchStrs = map stripDepth $ splitWhen (==(0, '|')) (withBranchDepth str)

withBranchDepth :: String -> [(Int, Char)]
withBranchDepth str = zip (branchDepth str) str

branchDepth :: String -> [Int]
branchDepth str = tail (scanl next 0 str)
  where next d '(' = d + 1
        next d ')' = d - 1
        next d _   = d

stripDepth :: [(Int, Char)] -> String
stripDepth = map snd
