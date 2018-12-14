import Data.Vector ((!), Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MV
import Control.Monad (forM_)

type Pt = (Int, Int)

cellValue :: Int -> Pt -> Int
cellValue serial (x, y) = hundreds ((rackId * (y + 1) + serial) * rackId) - 5
  where rackId = x + 11
        hundreds n = (n `div` 100) `mod` 10

walkGrid :: Int -> [Pt]
walkGrid n = [ (x, y) | y <- [0..n - 1],
                        x <- [0..n - 1] ]

gridSum :: Vector Int -> Int -> Pt -> Int -> Int
gridSum sumTable n (x, y) size = val - top - left + diag
    where i = (x - 1) + (y - 1) * n
          diag = if x > 0 && y > 0 then sumTable ! i              else 0
          top =  if y > 0          then sumTable ! (i + size)     else 0
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

fuelGridWalk3x3 :: Vector Int -> Int -> [(Int, Pt)]
fuelGridWalk3x3 sumTable n = map withValues pts
  where pts = walkGrid (n - 2)
        withValues pt = (gridSum sumTable n pt 3, pt)

part1 :: Vector Int -> Int -> Pt
part1 sumTable n = snd . maximum $ fuelGridWalk3x3 sumTable n


main = do
  let serial = 8199
  let n = 300

  sumTable <- genSumTable (cellValue serial) n

  let add (x, y) = (x + 1, y + 1)
  print $ add (part1 sumTable n) -- 235,87

