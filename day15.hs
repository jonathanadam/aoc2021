import System.IO 
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set 

type Coord = (Int, Int)
type Score = Int 

infinity = 922337203685477500

data SetEntry = SetEntry Score Coord deriving (Eq, Ord)

updateList::  Map.Map Coord Score -> Set.Set SetEntry -> [(Coord, Score)] -> Set.Set SetEntry
updateList scores set coordScores = foldr (\(coord, score) s -> Set.insert (SetEntry score coord) (Set.delete (SetEntry (oldScore coord) coord) s)) set coordScores 
  where oldScore coord = fromMaybe infinity $ Map.lookup coord scores 

makeMap:: [[Int]] -> Map.Map Coord Int
makeMap alon = Map.fromList $ concat $ map (\(y, xVals) -> map (\(x, val) -> ((x, y), val)) $ zip [1, 2..] xVals) $ zip [1,2..] alon

expandFullMap ::[[Int]] -> Map.Map Coord Int
expandFullMap alon = Map.fromList $ concat $ map (\(y, xVals) -> map (\(x, val) -> ((x, y), val)) $ zip [1, 2..] xVals) $ zip [1,2..] (expandOneBlock (map expandOneLine alon))

expandOneLine :: [Int] -> [Int]
expandOneLine l = concat $ take 5 $ iterate (map ((\x -> (x `mod` 9)+1 ))) l 

expandOneBlock :: [[Int]] -> [[Int]]
expandOneBlock b = concat $ take 5 $ iterate (map (map ((\x -> (x `mod` 9) + 1 )))) b

neighbors:: Coord -> [Coord]
neighbors (i,j) = [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]

findPath:: Map.Map Coord Int -> Coord -> Coord -> Maybe Score
findPath resistances a b = dijkstra 
                              resistances 
                              a 
                              b 
                              (Map.singleton a 0) 
                              (Set.insert 
                                (SetEntry 0  a)  
                                (Set.fromList 
                                  $ map (\x -> SetEntry infinity x)  
                                  (filter (/=a) (Map.keys resistances))))

dijkstra:: Map.Map Coord Int -> Coord -> Coord -> Map.Map Coord Int -> Set.Set SetEntry -> Maybe Score 
dijkstra resistances a b scores unvisited 
 | Set.null unvisited = Map.lookup b scores
 | Set.notMember (SetEntry (fromMaybe infinity $ Map.lookup b scores) b) unvisited = Map.lookup b scores
 | otherwise = dijkstra resistances newA b (Map.insert a scoreA newScores) (updateList scores (Set.delete (SetEntry scoreA a) unvisited) closestNeighbors) 
  where 
    closestNeighbors = map (\coord -> (coord, coordScore coord)) 
      $ filter (\ x -> Set.member (SetEntry (oldScore x) x) unvisited) (neighbors a)
    newScores = foldr (\(x, y) m -> Map.insert x y m) scores closestNeighbors 
    SetEntry lowestDistance newA= fromMaybe (SetEntry infinity a) $ Set.lookupMin unvisited
    scoreA = fromMaybe infinity $ Map.lookup a scores
    oldScore coord = fromMaybe infinity $ Map.lookup coord scores
    coordScore coord = min (oldScore coord) (scoreA + (fromMaybe infinity $ Map.lookup coord resistances))
  

findMinScore:: Map.Map Coord Int -> Maybe Score
findMinScore resistances = findPath resistances (1,1) $ maximum $ Map.keys resistances 

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let resistances = makeMap $ map (map digitToInt) inputLines
  let largerMap = expandFullMap $ map (map digitToInt) inputLines
  print (findMinScore resistances)
  print (findMinScore largerMap)
  hClose handle