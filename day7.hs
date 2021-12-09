import System.IO 
import Data.List
import Data.Function

stringToInt x = read x ::Int 

splitOn:: (Eq a)=> a -> [a] -> [[a]]
splitOn _ [] = []
splitOn a l@(x:xs)
  | a== x = splitOn a xs
  | otherwise = let (h, t) = break (a==) l in h:(splitOn a t)

distances :: Int -> [Int] -> [Int]
distances x xs = map (\y -> sumDistances $ (max x y ) - (min x y)) xs 

sumDistances 0 = 0
sumDistances 1 = 1
sumDistances x =  (x * (x+1)) `div` 2

minDistance xs = minimumBy (compare `on` snd) [(a, sum $ distances a xs) | a <- [minimum xs.. maximum xs]]

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let crabs = map stringToInt (splitOn ',' contents)
  print (minDistance crabs)
  hClose handle
