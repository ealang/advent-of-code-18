module Parse (parseInput) where

import Data.Function (on)
import Data.List (groupBy)
import Data.List.Split (splitWhen)

import Types (Direction(..), Element(..))

parseInput :: String -> Element
parseInput str = Sequence (map parseGroup groups)
  where groups = groupByElement str
        parseGroup group = if head group == '('
                           then parseAsBranch group
                           else Directions (map parseDirection group)

parseDirection :: Char -> Direction
parseDirection 'N' = N
parseDirection 'E' = E
parseDirection 'S' = S
parseDirection 'W' = W

parseAsBranch :: String -> Element
parseAsBranch str = Branch branches
  where branches = map parseInput branchStrs
        branchStrs = splitOnSeparators (stripParens str)
        stripParens = tail . init

groupByElement :: String -> [String]
groupByElement str = stripDepth $ groupBy rootLevel (withElementGroup str)
  where rootLevel = (==) `on` fst
        stripDepth = map (map snd)

splitOnSeparators :: String -> [String]
splitOnSeparators str = stripDepth $ splitWhen isSeparator (withBranchDepth str)
  where isSeparator (d, c) = d == 0 && c == '|'
        stripDepth = map (map snd)

withBranchDepth :: String -> [(Int, Char)]
withBranchDepth str = zip (branchDepth str) str

withElementGroup :: String -> [(Int, Char)]
withElementGroup str = zip (segmentNum str) str

segmentNum :: String -> [Int]
segmentNum = depth 0 0
  where depth 0 n ('(':xs) = n + 1 : depth 1 (n + 1) xs
        depth d n ('(':xs) = n : depth (d + 1) n xs
        depth 1 n (')':xs) = n : depth 0 (n + 1) xs
        depth d n (')':xs) = n : depth (d - 1) n xs
        depth d n (c  :xs) = n : depth d n xs
        depth _ _ []       = []

branchDepth :: String -> [Int]
branchDepth str = tail (scanl next 0 str)
  where next d '(' = d + 1
        next d ')' = d - 1
        next d _   = d
