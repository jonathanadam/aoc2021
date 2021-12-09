import System.IO 
import Data.Char
import Data.List
import Data.Maybe

type BingoRow = [Int]
type BingoBoard = [BingoRow]

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

makeBingoRows :: BingoBoard -> BingoBoard
makeBingoRows l =  l ++ transpose l

isBoardWon :: BingoBoard -> Bool
isBoardWon b = isJust $ find null b

coverMove :: BingoBoard -> Int -> BingoBoard
coverMove board move = map (filter (\x -> x /= move)) board

playBoards :: [BingoBoard] -> Int -> [BingoBoard]
playBoards bs move = map (\b -> coverMove b move) bs

findWinner :: [BingoBoard] -> Maybe BingoBoard 
findWinner bs = find isBoardWon bs

findWinningMove :: [BingoBoard] -> [Int] -> (BingoBoard, Int)
findWinningMove bs (x:xs)  = 
  case (findWinner playMove) of 
    Just b  -> (b, x)
    Nothing -> findWinningMove playMove xs
  where playMove = playBoards bs x
findWinningMove _ [] = ([], -1)

findLosingBoard :: [BingoBoard] -> [Int] -> (BingoBoard, Int)
findLosingBoard [] _ = ([[1]], -100000)
findLosingBoard b [] = (head b, -1)
findLosingBoard [b] xs = findWinningMove [b] xs
findLosingBoard (b:bs) (x:xs) = findLosingBoard (filter (\board -> not $ isBoardWon board) $ playBoards (b:bs) x) xs


remainingProduct (b, m) = m * ( foldl  (+) 0  ( nub (concat b)))

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputs = lines contents
  let moves = map read $ splitOn ',' (inputs!!0) :: [Int]
  let boards = map makeBingoRows $ splitEvery 5 $ map (map stringToInt) $ filter (\x -> not(null x)) $ map words $ tail inputs
  print (findWinningMove boards moves)
  print( remainingProduct $ findWinningMove boards moves)
  print (findLosingBoard boards moves)
  print(remainingProduct $ findLosingBoard boards moves)
  hClose handle

