import System.IO 
import Data.List

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

fishCensus fish = map (\x ->  length $ filter (==x) fish) [0..8]
nextGenerationCensus census = (take 6 (tail census)) ++ [census!!7 + census !!0] ++ (drop 7 (tail census)) ++ [census!!0]
nthCensus census n = (iterate nextGenerationCensus census)!!n
nthFishNumber fish n = sum $ nthCensus (fishCensus fish )n

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let initialFish = map stringToInt (splitOn ',' contents)
  print (nthFishNumber initialFish 80)
  print (nthFishNumber initialFish 256) 
  hClose handle