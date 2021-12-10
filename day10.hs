import System.IO 
import Data.List
import Data.Function
import Data.Maybe
import Data.Char

consumeLine [] y parseReturn = parseReturn [] y
consumeLine s@(x:xs) accum parseReturn = case x of 
  '(' -> consumeLine xs (')':accum) parseReturn
  '[' -> consumeLine xs (']':accum) parseReturn
  '<' -> consumeLine xs ('>':accum) parseReturn
  '{' -> consumeLine xs ('}':accum) parseReturn
  otherwise -> parseReturn s accum 

closeBracket []  _ = Nothing 
closeBracket (x:xs) []  = Just x
closeBracket (x:xs) (y:ys) = if (x == y) then consumeLine xs ys closeBracket else Just x

autoComplete [] [] = Nothing
autoComplete [] l = Just l 
autoComplete (x:xs) (y:ys) = if (x == y) then consumeLine xs ys autoComplete else consumeLine (x:xs) (y:ys) autoComplete

findParseError line = consumeLine line [] closeBracket
findAutoComplete line = consumeLine line [] autoComplete

scoreSingleChar x =  case x of 
   ')'  -> 1
   ']' -> 2
   '}' -> 3
   '>' -> 4
scoreAutoComplete xs = foldl (\x y -> 5*x + scoreSingleChar y) 0 xs

scoreParseErrors x = case x of 
  Nothing -> 0
  Just ')' -> 3 
  Just ']' -> 57 
  Just '}' -> 1197 
  Just '>' -> 25137 

scoreAutoCompletes xs = (sort $ map scoreAutoComplete xs )!!((length xs) `div` 2)
  
main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let parseScores = map (scoreParseErrors . findParseError) inputLines
  print (sum parseScores)

  let incompleteLines = filter (\x -> ((scoreParseErrors . findParseError) x) == 0) inputLines
  let autoCompletes = map ((fromMaybe "" ).findAutoComplete) incompleteLines
  let middleScore = scoreAutoCompletes autoCompletes
  print middleScore
  hClose handle