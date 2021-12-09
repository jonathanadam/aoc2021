import System.IO 
import Data.List
import Data.Function
import Data.Maybe

stringToInt x = read x ::Int 

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

--splitOn:: (Eq a)=> a -> [a] -> [[a]]
splitOn _ [] = []
splitOn a l@(x:xs)
  | a== x = splitOn a xs
  | otherwise = let (h, t) = break (a==) l in h:(splitOn a t)

countOnesFoursAndSevens input = length $ filter (\x -> (length x )`elem` [2, 3,4, 7])(snd input) 

findZero xs = sort $ fromMaybe "" $ find (\x -> (length x == 6) && (x /= six) &&( x /= nine)) xs where 
  six = findSix xs
  nine = findNine xs
findOne xs = sort $ fromMaybe "" $ find (\x -> (length x) == 2) xs
findTwo xs = sort $ fromMaybe "" $ find (\x -> (length x == 5) && length (intersect x four) == 2) xs where four = findFour xs
findThree xs = sort $ fromMaybe "" $ find (\x -> (length x == 5) && length (intersect x one) == 2) xs where one = findOne xs
findFour xs = sort $ fromMaybe "" $ find (\x -> length x == 4) xs
findFive xs =  sort $ fromMaybe "" $ find (\x -> (length x == 5) && not (x `elem` [two, three]) ) xs where 
  three = findThree xs
  two = findTwo xs
findSix xs = sort $ fromMaybe "" $ find (\x -> (length x == 6) && length (intersect x one) == 1) xs where one = findOne xs
findSeven xs = sort $ fromMaybe "" $ find (\x -> length x == 3) xs
findEight xs = sort $ fromMaybe "" $ find (\x -> length x == 7) xs
findNine xs = sort $ fromMaybe "" $ find (\x -> (length x == 6) && (length (intersect x four) == 4 )) xs where four = findFour xs

findCode xs = [( (findOne ys), '1'), (findTwo ys, '2'), (findThree ys, '3' ), (findFour ys, '4'), (findFive ys, '5'), ( findSix ys, '6'), ( findSeven ys, '7'), (findEight ys, '8'), ( findNine ys, '9'), ( findZero ys, '0')] where ys = map sort xs

--decode:: ([String], [String]) -> String
decode input = let dict = findCode $ fst input in map (\x -> fromMaybe ' ' $ lookup (sort x) dict) $ snd input
--map (\x -> fromMaybe ' ' $ lookup x $ findCode (fst input)) $ snd input


main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let inputs = map ((\x-> (take 10 x, drop 11 x)) . words ) inputLines
  print (sum $ map (stringToInt . decode) inputs)
  --print (findCode (fst (inputs!!3)))
  --print (map sort $ snd (inputs!!3))
  --print (map decode inputs)
  hClose handle
