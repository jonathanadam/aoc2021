import System.IO 
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

stringToInt x = read x ::Int 

splitOn:: (Eq a)=> a -> [a] -> [[a]]
splitOn _ [] = []
splitOn a l@(x:xs)
  | a== x = splitOn a xs
  | otherwise = let (h, t) = break (a==) l in h:(splitOn a t)

parseInstruction instruction = let instList = splitOn '=' ((last.words) instruction) in case head instList of 
 "x" -> (stringToInt ((head.tail) instList), 0)
 "y" -> (0,stringToInt ((head.tail) instList))

parseCoord inputLine = (stringToInt (i!!0), stringToInt (i!!1)) where i = splitOn ',' inputLine

foldCoord 0 coord = coord
foldCoord f coord  = if coord > f then  (2*f) - coord else coord

foldDot (foldX, foldY) (dotX, dotY) = (foldCoord foldX dotX, foldCoord foldY dotY)

mapDots dots = Map.fromList (zip dots $ repeat 'X')
makeFold dotmap fold = Map.mapKeys (foldDot fold) dotmap
makeFolds instructions dotmap = foldl makeFold dotmap instructions

printDots dotmap = do
  let keys = Map.keys dotmap
  let (minX, maxX) = (minimum xs, maximum xs) where xs = map fst keys
  let (minY, maxY) = (minimum ys, maximum ys) where ys = map snd keys 
  let dotLines = [ [fromMaybe ' ' (Map.lookup (x, y) dotmap) | x <-[minX..maxX] ]| y<- [minY..maxY]]
  mapM putStrLn dotLines

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let (inputDots, inputFolds) = (a, tail b) where (a, b) = break (=="") inputLines
  let instructions = map parseInstruction inputFolds
  let coords = map parseCoord inputDots
  let dotmap = mapDots coords
  let firstFold = makeFold   dotmap (instructions!!0)
  print (Map.size firstFold)
  let fullFold = makeFolds instructions dotmap
  printDots fullFold
  hClose handle