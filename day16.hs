import System.IO 
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set 

takeUntil cond xs = a ++ [head b] where (a, b) = span cond xs 

expandToHex ::  String -> [Int]
expandToHex l =  concat $ map charToHex l

binToInt :: [Int] -> Int
binToInt [] = 0
binToInt [0] = 0
binToInt [1] = 1
binToInt (x:xs) = (2*x)^(length xs)+ binToInt xs

charToHex :: Char -> [Int]
charToHex x = case x of 
  '0' -> [0,0,0,0]
  '1' -> [0,0,0,1]
  '2' -> [0,0,1,0]
  '3' -> [0,0,1,1]
  '4' -> [0,1,0,0]
  '5' -> [0,1,0,1]
  '6' -> [0,1,1,0]
  '7' -> [0,1,1,1]
  '8' -> [1,0,0,0]
  '9' -> [1,0,0,1]
  'A' -> [1,0,1,0]
  'B' -> [1,0,1,1]
  'C' -> [1,1,0,0]
  'D' -> [1,1,0,1]
  'E' -> [1,1,1,0]
  'F' -> [1,1,1,1] 

parse string = decodePacketHeader (0, string)

greaterFromList:: [Int] -> Int
greaterFromList l@[a,b] = if a > b then 1 else 0
smallerFromList:: [Int] -> Int
smallerFromList l@[a,b] = if a < b then 1 else 0
equalFromList:: [Int] -> Int
equalFromList l@[a,b] = if a == b then 1 else 0

decodePacketHeader :: (Int, [Int]) -> (Int, [Int])
decodePacketHeader (a, string) = case binToInt packetId of 
  0 -> parseOperator((foldl (+) 0), r2 )
  1 -> parseOperator((foldl (*) 1), r2)
  2 -> parseOperator (minimum, r2)
  3 -> parseOperator (maximum, r2)
  4 -> parseLiteralValue (0, r2)
  5 -> parseOperator (greaterFromList,r2)
  6 -> parseOperator (smallerFromList,r2)
  7 -> parseOperator (equalFromList,r2)
  where (packetVersion, r1) = splitAt 3 string
        (packetId, r2) = splitAt 3 r1

parseLiteralValue:: (Int, [Int]) -> (Int, [Int])
parseLiteralValue (a, 0:xs) = (a*(2^4) + (binToInt val), rest) where (val, rest) = splitAt 4 xs
parseLiteralValue (a, 1:xs) = parseLiteralValue (a*2^4 + (binToInt val), rest) where (val, rest) = splitAt 4 xs

parseOperator :: ([Int]->Int, [Int]) -> (Int, [Int])
parseOperator (f, 0:xs) = (f b, subrest)
  where (subblockSize, r1) = splitAt 15 xs 
        (subblock, subrest) = splitAt (binToInt subblockSize) r1
        b = map fst $ takeUntil (\(x,y) -> not (isEndParse y)) $ tail $ iterate decodePacketHeader (0, subblock)
parseOperator (f, 1:xs) = (f b, finalRest)
  where (numParses, rest) = splitAt 11 xs
        numSubblocks = binToInt numParses
        l = take (numSubblocks) $ tail $ iterate decodePacketHeader (0, rest)
        b = map fst l
        finalRest = (snd.last) l

isEndParse :: [Int] -> Bool
isEndParse y = ((null y) || (all (==0) y))

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let hexLine = expandToHex contents
  print (parse hexLine)
  hClose handle