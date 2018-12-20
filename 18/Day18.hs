import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.Map ((!), Map)
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Settlers

cacheStates :: Int -> Terrain -> (Int, Int, Map Int Terrain)
cacheStates n terrain = find (statesFrom n terrain) Map.empty 0
  where generations = statesFrom n terrain
        invertMap = Map.fromList . map swap . Map.toList
        find (t:xs) hist i = case Map.lookup t hist of
                               Just start -> (start, i - start, invertMap hist)
                               Nothing    -> find xs (Map.insert t i hist) (i + 1)

lookupState n terrain = fromCache (cacheStates n terrain)
  where fromCache (start, len, hist) i =
          hist ! (if i < start then i
                  else start + ((i - start) `mod` len))

displayGeneration n lookup i = 
  putStrLn $ "Generation= " ++ show i ++ " " ++
             "Value= " ++ show (resourceValue terrain) ++ "\n" ++
             renderTerrain n terrain ++ "\n"
    where terrain = lookup i

main = do
  let findSize = floor . sqrt . fromIntegral . length
  (n, terrain) <- (findSize &&& Vector.fromList) . filter (/='\n') <$> readFile "input.txt"

  let lookup = lookupState n terrain

  displayGeneration n lookup 10
  displayGeneration n lookup 1000000000

  putStrLn "[Hit enter]"
  getLine

  forM_ [0..] (displayGeneration n lookup)
