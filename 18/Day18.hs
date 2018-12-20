{-# LANGUAGE ScopedTypeVariables #-}
import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.Map ((!), Map)
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Settlers

-- Find value after 10 generations
part1 n terrain = do
  let terrain' = statesFrom n terrain !! 10
  putStrLn (renderTerrain n terrain')
  print $ resourceValue terrain'

findLoop :: Int -> Terrain -> (Int, Int, Map Int Terrain)
findLoop n terrain = loop (statesFrom n terrain) Map.empty 0
  where generations = statesFrom n terrain
        invertMap :: Map (Vector Char) Int -> Map Int (Vector Char) = Map.fromList . map swap . Map.toList
        loop (t:xs) hist i = case Map.lookup t hist of
                               Just start -> (start, i - start, invertMap hist)
                               Nothing     -> loop xs (Map.insert t i hist) (i + 1)

-- Find value after n generations
part2GenVal n terrain i =
  putStrLn $ "Loop start= " ++ show start ++ " " ++ 
             "Length= " ++ show len ++ " " ++
             "Value= " ++ show (resourceValue $ hist ! loopIndex) ++ "\n" ++
             renderTerrain n (hist ! loopIndex)
  where (start, len, hist) = findLoop n terrain
        loopIndex = start + ((i - start) `mod` len)

part2Vis n terrain =
  forM_ (zip [0..] (statesFrom n terrain)) showGen
  where showGen (i, terrain') = putStrLn $ "Gen= " ++ show i ++ " " ++
                                           "Value= " ++ show (resourceValue terrain') ++ "\n" ++
                                           renderTerrain n terrain' ++ "\n"

main = do
  let findSize = floor . sqrt . fromIntegral . length
  (n, terrain) <- (findSize &&& Vector.fromList) . filter (/='\n') <$> readFile "input.txt"
  part1 n terrain
  putStrLn "[Hit enter]"
  getLine
  part2GenVal n terrain 1000000000 
  putStrLn "[Hit enter]"
  getLine
  part2Vis n terrain
