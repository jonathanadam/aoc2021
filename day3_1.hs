import System.IO 
import Data.Char
import Data.List

binaryToDec :: [Int] -> Int
binaryToDec xs = foldl 
                  (\x y -> x + 2^(fst y)) 
                  0 
                  (filter (\tup -> snd tup > 0 ) 
                  (zip [0,1..] $ reverse xs))
countOnesAndZeros :: (Int, Int) -> Char -> (Int, Int)
countOnesAndZeros x y = if y =='1'
  then (fst x, snd x + 1)
  else (fst x+1 ,snd x)

onesAndZeros xs  = map (foldl countOnesAndZeros (0,0)) $ transpose xs 

calculateGamma xs = binaryToDec $ map (\x -> if fst x > snd x then 0 else 1) xs
calculateEpsilon xs = binaryToDec $ map (\x -> if fst x < snd x then 0 else 1) xs


main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let coords = lines contents
  let counts =  onesAndZeros coords
  let gammaDec =  calculateGamma counts
  let epsilonDec =  calculateEpsilon counts
  print (gammaDec*epsilonDec)
  hClose handle

