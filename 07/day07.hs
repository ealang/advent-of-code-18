import Data.Char (ord)
import Data.List (intercalate)
import Data.Map ((!), Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

taskTime :: String -> Int
taskTime task = 61 + (ord . head $ task) - ord 'A'

tasksAvail :: Map String [String] -> Set String -> Set String -> Set String
tasksAvail deps completed = Set.filter canRun
  where canRun task = not $ any (`Set.notMember` completed) (Map.findWithDefault [] task deps)

allTasksFrom :: Map String [String] -> Set String
allTasksFrom deps = Set.fromList $ concatMap (uncurry (:)) (Map.toList deps)

-- Return list of tasks with completion time
execTasks :: Map String [String] -> Int -> [(Int, String)]
execTasks deps nworkers = exec 0 nworkers (allTasksFrom deps) Set.empty Set.empty
  where exec time n avail completed pending 
          | null pending && null avail = []
          | n == 0 || null canRun      = wait
          | otherwise                  = runNext
          where wait = event:exec time' (n + 1) avail completed' pending'
                       where completed' = task `Set.insert` completed
                             (event@(time', task), pending') = Set.deleteFindMin pending
                runNext = exec time  (n - 1) avail' completed pending'
                          where task = Set.findMin canRun
                                event   = (time + taskTime task, task)
                                pending' = event `Set.insert` pending
                                avail'  = task `Set.delete` avail
                canRun = tasksAvail deps completed avail

-- Get execution order
part1 :: Map String [String] -> Int -> [String]
part1 deps nWorkers = map snd $ execTasks deps nWorkers

-- Get total time to execute tasks
part2 :: Map String [String] -> Int -> Int
part2 deps nWorkers = fst . last $ execTasks deps nWorkers

main = do
  let parseStep step = (a, b)
        where _:a:_:_:_:_:_:b:_ = words step

  steps <- map parseStep . lines <$> readFile "day07.txt"
  let deps = foldl (\order' (a, b) -> Map.insertWith (++) b [a] order')
                   Map.empty steps

  print $ intercalate "" (part1 deps 1)
  print $ part2 deps 5
