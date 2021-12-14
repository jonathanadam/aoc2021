import System.IO 
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

endChar = '/'
type Bond = String
type Count = Int

parse:: String -> (Bond, [Bond])
parse rule = let [a,b] = take 2 rule; x = last rule in ([a,b], [[a,x],[x,b]])
makeRules ruleset = Map.fromList ruleset

grow :: Map.Map Bond [Bond] -> Map.Map Bond Count ->  Map.Map Bond Count
grow rs bs = Map.foldrWithKey (addBonds rs) Map.empty bs

addBonds :: Map.Map Bond [Bond] -> Bond -> Count -> Map.Map Bond Count -> Map.Map Bond Count
addBonds rs bond count bs  = foldr 
                                        (\x c -> Map.insertWith (+) x count c)
                                        bs 
                                        (fromMaybe [bond] (Map.lookup bond rs))

nthPolymer:: Int -> Map.Map Bond [Bond] -> Map.Map Bond Count -> Map.Map Bond Count
nthPolymer n rs bs = head $ drop n $ iterate (grow rs) bs

countBonds :: String -> Map.Map Bond Int
countBonds polymer = Map.fromListWith (+) $ zip polyPairs (repeat 1) 
  where 
    polyPairs = map (\(a,b) -> [a,b]) 
              $ zip padded ((tail padded) ++ [endChar]) 
    padded = endChar:polymer

countElements :: Map.Map Bond Int -> Map.Map Char Int
countElements bondCount = Map.map (\x-> x `div` 2)  
                        $ Map.delete endChar 
                        $ Map.foldrWithKey 
                            (\key value charCount -> 
                                foldr 
                                (\k c -> Map.insertWith (+) k value c) 
                                charCount 
                                key) 
                            Map.empty 
                            bondCount

scorePolymer:: Map.Map Bond Count -> Int
scorePolymer polymer =  let 
                          elements = Map.elems $ countElements polymer 
                        in 
                          (maximum elements) - minimum elements

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let (template, insertionEquations) = (head a, tail b) 
                                        where (a, b) = break (=="") inputLines
  let ruleset = map parse insertionEquations
  let rules = makeRules ruleset
  let bs = countBonds template 
  print (scorePolymer $ nthPolymer 40 rules bs)
  hClose handle