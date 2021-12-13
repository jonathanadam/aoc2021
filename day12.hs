import System.IO 
import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

type Vertex = String 
type Graph = [Edge] 
type Edge = Set.Set Vertex

neighbor vertex edge =  fromMaybe "" $ find (/= vertex) $ Set.toList edge

findNeighbors vertex graph = map (neighbor vertex) $ filter (Set.member vertex) graph

makeGraph edges = map (\(a,b) -> Set.fromList [a, b]) edges

--ITS SLOW
countPaths start end graph = countPathHelper start end [] graph
countPathHelper start end accum graph = if end == start then 1 else sum [countPathHelper x end (start:accum) newGraph | x <- validNeighbors]
  where 
        validNeighbors =   findValidNeighbors start graph (start:accum)
        newGraph = makeNewGraph start graph accum

findValidNeighbors vertex graph path = if (hasDuplicateSmall path) 
  then filter (\x -> ((isUpper.head) x) || (not (x `elem` path))) neighbors 
  else neighbors 
  where neighbors = findNeighbors vertex graph

makeNewGraph vertex graph accum = if ((isLower (head vertex)) && ((vertex `elem` (["start", "end"] ++ accum)))) then (filter (not.(Set.member vertex)) graph) else graph

hasDuplicateSmall accum = hasDupe . sort $ filter (isLower.head) accum

hasDupe [] = False
hasDupe [x] = False
hasDupe (a:b:xs) = if a==b then True else hasDupe (b:xs)

  
main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let edgeList = map ((\(a,b) -> (a, tail b)) . (break (=='-'))) inputLines
  print (makeGraph edgeList)
  let graph = makeGraph edgeList
  print (countPaths "start" "end" graph)
  hClose handle
