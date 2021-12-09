import System.IO 
import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

--makeMap:: [[Int]] -> Data.Map (Int, Int) Int
makeMap alon = Map.fromList $ concat $ map (\(y, xVals) -> map (\(x, val) -> ((x, y), val)) $ zip [1, 2..] xVals) $ zip [1,2..] alon

isLowPoint:: (Int, Int) -> Map.Map (Int, Int) Int -> Bool 
isLowPoint coords oceanMap = let  
  pointVal = fromMaybe 10 $ Map.lookup coords oceanMap
  vals = map (\x -> fromMaybe 11 $ Map.lookup x oceanMap) $ neighbors coords
  in (pointVal < (minimum vals))

findLowPoints oceanMap = Map.filterWithKey (\ coords _-> isLowPoint coords oceanMap) oceanMap

neighbors (i,j) = [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

addToBasin lowPoint oceanMap = accumToBasin [lowPoint] Map.empty oceanMap

accumToBasin [] basin _ = basin
accumToBasin (coord:xs) basin oceanMap = case Map.lookup coord basin of 
  Just _ -> accumToBasin xs basin oceanMap
  Nothing -> case Map.lookup coord oceanMap of 
    Just 9 -> accumToBasin xs basin oceanMap
    Nothing -> accumToBasin xs basin oceanMap
    Just x -> accumToBasin ((neighbors (coord)) ++ xs) (Map.insert coord x basin) oceanMap

basins oceanMap = map (\lowPoint -> addToBasin lowPoint oceanMap) ( Map.keys (findLowPoints oceanMap))

sizeBasins oceanMap = map Map.size $ basins oceanMap

topThreeBasins oceanMap = take 3 $ reverse.sort $ sizeBasins oceanMap

sumRisk oceanMap = Map.foldr (\x y -> x + 1 + y ) 0 $ findLowPoints oceanMap
  
main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let inputs = map (map digitToInt) inputLines
  let oceanFloor = makeMap inputs
  print (product $ topThreeBasins oceanFloor)
  hClose handle
