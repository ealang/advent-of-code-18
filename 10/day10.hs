import Data.List (minimumBy)
import Data.Ord (comparing)
import Text.Regex.Posix ((=~), getAllTextMatches)

type Vec2 = (Int, Int)
type BBox = (Vec2, Int, Int)

bboxWidth :: [Vec2] -> Int
bboxWidth pts = maximum xvals - minimum xvals 
  where xvals = map fst pts

bbox :: [Vec2] -> BBox
bbox pts = (tl, bboxWidth pts, height)
  where tl = (minimum xvals, minimum yvals)
        height = maximum yvals - minimum yvals
        xvals = map fst pts
        yvals = map snd pts

particleSim :: [Vec2] -> [Vec2] -> [[Vec2]]
particleSim vels = iterate (applyVel vels)
  where applyVel = zipWith addVel
        addVel (vx, vy) (x, y) = (x + vx, y + vy)

-- Run the particle sim for a while, returning the state where
-- the spread of the particles is minimized.
part1 pts vels = minimumBy (comparing bboxWidth) (take n sim)
  where sim = particleSim vels pts
        n = bboxWidth pts `div` 2

renderPts :: [Vec2] -> String
renderPts pts = unlines [unwords [char (x, y) |
                                   x <- [sx..sx + w]] |
                                   y <- [sy..sy + h]]
  where ((sx, sy), w, h) = bbox pts
        char pt = if pt `elem` pts then "X" else " "

parseLine :: String -> (Vec2, Vec2)
parseLine line = ((readInt x, readInt y), (readInt vx, readInt vy))
  where [x, y, vx, vy] = getAllTextMatches $ line =~ "-?[0-9]+" :: [String]
        readInt s = read s :: Int

main = do
  (pts, vels) <- unzip .
                 map parseLine .
                 lines <$> readFile "day10.txt"
  putStrLn $ renderPts (part1 pts vels)
