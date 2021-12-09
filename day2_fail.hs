import System.IO 
import Data.Char

data SubCoord =  SubCoord {
  x :: Int,
  y :: Int,
  aim :: Int} deriving Show


mapCoord :: [Char] -> SubCoord -> SubCoord
mapCoord s  SubCoord{x=x, y=y, aim=aim}
  |head s == 'u' = SubCoord x (y) (aim - movement)
  |head s == 'f' = SubCoord (x + movement) (y + aim*movement) aim
  |head s == 'd' = SubCoord x (y) (aim + movement)
  | otherwise = SubCoord x y aim
  where movement = digitToInt $ last s



mapCoords :: [String] -> SubCoord
mapCoords [] = SubCoord 0 0 0
mapCoords (hd:tl) = mapCoord hd $ mapCoords tl

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let coords = mapCoords $ lines contents
  print $ x coords * y coords
  hClose handle

-- 1508914963
-- 1507390213