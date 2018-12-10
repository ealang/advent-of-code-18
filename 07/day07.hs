import Data.Map ((!), Map)
import Data.Tuple (swap)
import Data.List (sort, intercalate, find)
import Data.Set (Set)
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set

invertGraph :: Map String [String] -> Map String [String]
invertGraph graph = foldl insert Map.empty pairs
  where pairs                = concatMap (\(k, vals) -> [(v, k) | v <- vals]) $ Map.toList graph
        insert graph' (k, v) = Map.insertWith (++) k [v] graph'

findLeaf :: Map String [String] -> String
findLeaf graph = backFrom start
  where (start, _) = Map.findMin graph
        backFrom node
          | Map.member node graph = backFrom (head $ graph ! node)
          | otherwise             = node

findLeaves :: Map String [String] -> String -> Set String
findLeaves graph node
  | Map.notMember node graph = Set.singleton node
  | otherwise                = foldl Set.union Set.empty
                                     [findLeaves graph node | node <- graph ! node ]

-- Find alphabetically sorted run order of tasks
part1 :: Map String [String] -> [String]
part1 order = part1' startingNodes Set.empty
  where deps          = invertGraph order
        startingNodes = findLeaves deps (findLeaf order)
        part1' consider completed
          | Set.null consider = []
          | otherwise         = node:part1' consider' completed'
              where Just node  = find canRun (Set.elems consider)
                    consider'  = Set.union (Set.delete node consider) (Set.fromList unlocked)
                    completed' = Set.insert node completed
                    unlocked   = Map.findWithDefault [] node order
                    canRun node         = not $ any (`Set.notMember` completed) (dependenciesOf node)
                    dependenciesOf node = Map.findWithDefault [] node deps

main = do
  let parseStep step = (a, b)
        where _:a:_:_:_:_:_:b:_ = words step

  steps <- map parseStep . lines <$> readFile "day07.txt"
  let order = foldl (\order' (a, b) -> Map.insertWith (++) a [b] order')
              Map.empty steps

  print $ intercalate "" (part1 order) -- BFLNGIRUSJXEHKQPVTYOCZDWMA
