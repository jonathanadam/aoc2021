import System.IO 
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map

stringToInt x = read x ::Int 

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

splitOn:: (Eq a)=> a -> [a] -> [[a]]
splitOn _ [] = []
splitOn a l@(x:xs)
  | a== x = splitOn a xs
  | otherwise = let (h, t) = break (a==) l in h:(splitOn a t)

type Coord = (Int, Int)
x :: Coord -> Int
y :: Coord -> Int
x a = fst a 
y a = snd a 

type CoordPair = (Coord, Coord)

straightLines coords = filter (\coordPair -> x (fst coordPair) == x (snd coordPair) || (y (fst coordPair) == y (snd coordPair)))coords

diagonalLines coords = filter (\(a, b) -> foldl (-)  0 (map x [a,b]) == foldl (-) 0  (map y [a, b])) coords

straightLineSegment coordPair@(start, end) = if (x start == x end ) then findLineSegmentX coordPair 
else if (y start == y end) then findLineSegmentY coordPair 
else findDiagonalLineSegment coordPair
findLineSegmentX coordPair@(start,end) = [(x start, newY) | newY <- [lo..hi]] where 
  lo = min (y start) (y end)
  hi = max (y start) (y end)
findLineSegmentY coordPair@(start,end) = [(newX, y start) | newX <- [lo..hi]] where 
  lo = min (x start) (x end)
  hi = max (x start) (x end)
findDiagonalLineSegment coordPair@(start,end) = zip [startX, (startX+xIncrement)..(x end)] [startY, (startY + yIncrement)..(y end)] where 
  startX = x start
  xIncrement = if (x end) > (x start) then 1 else -1 
  startY = y start 
  yIncrement = if (y end) > (y start) then 1 else -1 

addPointsToCounts:: Map.Map Coord Int -> [Coord]  -> Map.Map Coord Int
addPointsToCounts m  cs= foldl (\coordMap c -> Map.insertWith (+) c 1 coordMap) m cs

addLine:: Map.Map Coord Int -> CoordPair -> Map.Map Coord Int
addLine m c = addPointsToCounts m (straightLineSegment c)

addLines m cs = foldl addLine m cs

findIntersections cs =  Map.filter (>1)(addLines Map.empty cs)


main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputs =  map words $ lines contents
  let numberPairs = map (filter (\x -> x /= "->"))  inputs
  let readNumbers = map (map (splitOn ',')) numberPairs
  let numbers = [ map (map stringToInt)  x | x <- readNumbers]
  let coords = [((a!!0!!0, a!!0!!1),(a!!1!!0, a!!1!!1)) | a <- numbers]
  
  print (Map.size $ findIntersections coords) 
  -- let coords = [ ((a!!0, a!!1), (a!!2, a!!3))|a <- inputs]
  -- let rawCoords = map read . words $ inputs :: [Int]
  

  hClose handle

-- 16896