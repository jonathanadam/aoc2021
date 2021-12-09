import System.IO 
import Data.Char

data SubCoord =  SubCoord {
  x :: Int,
  y :: Int,
  aim :: Int} deriving Show

mapCoord :: SubCoord -> [Char] -> SubCoord
mapCoord SubCoord{x=x, y=y, aim=aim} s
  |head s == 'u' = SubCoord x (y) (aim - movement)
  |head s == 'f' = SubCoord (x + movement) (y + movement*aim) aim
  |head s == 'd' = SubCoord x (y) (aim + movement)
  | otherwise = SubCoord x y aim
  where movement = digitToInt $ last s

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let coords = foldl mapCoord  (SubCoord 0 0 0) $ lines contents
  print $ x coords * y coords
  hClose handle