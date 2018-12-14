type Pt = (Int, Int)

cellValue serial x y = hundreds ((rackId * y + serial) * rackId) - 5
  where rackId = x + 10
        hundreds n = (n `div` 100) `mod` 10

fuelGridWalk3x3 :: Int -> Int -> [([Int], Pt)]
fuelGridWalk3x3 serial n = map withValues pts
  where pts = [ (x, y) | y <- [1..n - 2],
                         x <- [1..n - 2] ]
        withValues pt = (valuesAt pt, pt)
        valuesAt (sx, sy) = [ cellValue serial x y | y <- [sy..sy + 2],
                                                     x <- [sx..sx + 2] ]

part1 :: Int -> Int -> Pt
part1 serial n = snd .
                 maximum .
                 map sumValues $ fuelGridWalk3x3 serial n
  where sumValues (values, pt) = (sum values, pt)

main = do
  let serial = 8199
  let n = 300
  print $ part1 serial n -- 235,87
