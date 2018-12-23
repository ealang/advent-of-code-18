import Control.Monad (forM_)
import Data.Vector ((!), Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MV

type Pt = (Int, Int)

cellValue :: Int -> Pt -> Int
cellValue serial (x, y) = hundreds ((rackId * (y + 1) + serial) * rackId) - 5
  where rackId = x + 11
        hundreds n = (n `div` 100) `mod` 10

walkGrid :: Int -> [Pt]
walkGrid n = [ (x, y) | y <- [0..n - 1],
                        x <- [0..n - 1] ]

gridSumFromTable :: Vector Int -> Int -> Pt -> Int -> Int
gridSumFromTable sumTable n (x, y) size = val - top - left + diag
    where i = (x - 1) + (y - 1) * n
          diag = if x > 0 && y > 0 then sumTable ! i              else 0
          top  = if y > 0          then sumTable ! (i + size)     else 0
          left = if x > 0          then sumTable ! (i + size * n) else 0
          val = sumTable ! (i + size + n * size)

genSumTable :: (Pt -> Int) -> Int -> IO (Vector Int)
genSumTable valueAt n = do cache <- MV.new (n * n)
                           forM_ (walkGrid n) (write cache)
                           Vector.freeze cache
  where write cache (x, y) = do
          let i = x + y * n
          diag <- if x > 0 && y > 0 then MV.read cache (i - n - 1) else return 0
          top  <- if y > 0          then MV.read cache (i - n)     else return 0
          left <- if x > 0          then MV.read cache (i - 1)     else return 0
          MV.write cache i (top + left - diag + valueAt (x, y))

fuelGridWalkMxM :: (Pt -> Int -> Int) -> Int -> Int -> [(Int, Pt)]
fuelGridWalkMxM gridSum n m = map withValues pts
  where pts = walkGrid (n - (m - 1))
        withValues pt = (gridSum pt m, pt)

part1 :: (Pt -> Int -> Int) -> Int -> Pt
part1 gridSum n = format . snd . maximum $ fuelGridWalkMxM gridSum n 3
  where format (x, y) = (x + 1, y + 1)

part2 :: (Pt -> Int -> Int) -> Int -> (Pt, Int)
part2 gridSum n = format . maximum $ [ (val, pt, m) | 
                                          m <- [1..n],
                                          (val, pt) <- fuelGridWalkMxM gridSum n m ]
  where format (_, (x, y), m) = ((x + 1, y + 1), m)

main = do
  let serial = 8199
  let n = 300

  sumTable <- genSumTable (cellValue serial) n
  let gridSum = gridSumFromTable sumTable n

  print $ part1 gridSum n
  print $ part2 gridSum n
