import System.IO 
import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

type OctopusMap = Map.Map (Int, Int) Int

makeMap alon = Map.fromList $ concat $ map (\(x, yVals) -> map (\(y, val) -> ((x, y), val)) $ zip [1, 2..] yVals) $ zip [1,2..] alon

neighbors (i,j) =  [(x, y) | x<- [i-1.. i+1], y <-[j-1.. j+1], (x,y) /= (i,j)]

step octoMap = flash $ Map.map (+1) octoMap

flash octoMap = until (\x -> null $ Map.filter (>9) x) flashPoints octoMap

reactToFlash (i,j) octoMap = case Map.lookup (i,j) octoMap of 
  Nothing -> octoMap
  Just x -> if x == 0 then octoMap else Map.insert (i,j) (x+1) octoMap

flashPoint (i,j) octoMap = foldr reactToFlash (Map.insert (i,j) 0 octoMap ) $ neighbors (i, j)

flashPoints octoMap = foldr flashPoint octoMap $ Map.keys (Map.filter (>9) octoMap)

countZeros octoMap stepNumber = sum $ map (\x -> Map.size $ Map.filter (==0) x) $ take (stepNumber + 1) (iterate step octoMap)

findSync octoMap = length. fst. span (\x -> not (all (== 0)(Map.elems x)))  $ iterate step octoMap
  
main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let inputs = map (map digitToInt) inputLines
  let octopi = makeMap inputs
  print (countZeros octopi 100)
  print (findSync octopi)
  hClose handle