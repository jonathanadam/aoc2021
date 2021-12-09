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

findMostCommonBit counts index = if fst (counts!!index) > snd (counts!!index) then '0' else '1'
findLeastCommonBit counts index = if (findMostCommonBit counts index)  == '0' then '1' else '0' 

findOxygenRating :: [[Char]] -> [Char]
findOxygenRating [a] = a 
findOxygenRating (hd:tl) = findOxygenRatingHelper (hd:tl)  0

findOxygenRatingHelper:: [[Char]]  ->  Int -> [Char]
findOxygenRatingHelper [a]  _ = a
findOxygenRatingHelper (hd:tl) i = 
  let counts = onesAndZeros (hd:tl) in 
            findOxygenRatingHelper 
            (filter 
            (\alon -> alon!!i == (findMostCommonBit counts i ))
            (hd:tl))
            $ succ i

findCarbonRating :: [[Char]] -> [Char]
findCarbonRating [a] = a 
findCarbonRating (hd:tl) = findCarbonRatingHelper (hd:tl)  0

findCarbonRatingHelper:: [[Char]] ->  Int -> [Char]
findCarbonRatingHelper [a] _ = a
findCarbonRatingHelper (hd:tl) i = 
    let counts = onesAndZeros (hd:tl) in 
            findCarbonRatingHelper 
            (filter 
            (\alon -> alon!!i == (findLeastCommonBit counts i))
            (hd:tl))
            $ succ i

oxygen xs = binaryToDec $ map digitToInt $ findOxygenRating xs
carbon xs = binaryToDec $ map digitToInt $  findCarbonRating xs 

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let readings = lines contents
  let oxygenDec = oxygen readings
  let carbonDec = carbon readings 
  print (carbonDec* oxygenDec )
  hClose handle

