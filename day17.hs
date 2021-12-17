import System.IO 
import Data.List
import Data.Maybe
import Data.Char

takeUntil cond xs = a ++ [head b] where (a, b) = span cond xs 

type Range = ((Int, Int), (Int, Int))
xMin ((a,_), _) = a
xMax ((_, a), _) = a
yMin (_, (a, _)) = a
yMax (_, (_, a)) = a

data Point = Point Int Int Int Int deriving (Show, Eq)
x (Point a _ _ _) = a
y (Point _ a _ _) = a
xVel (Point _ _ a _) = a
yVel (Point _ _ _ a) = a

step:: Point -> Point 
step (Point a b c d) = Point  (a+c)
                            (b + d)
                            (max 0 (c-1))
                            (d-1)

pointInRange:: Range ->  Point -> Bool
pointInRange ((lowX, highX), (lowY, highY)) (Point a b c d) =
  all (==True) [a >= lowX, a <= highX, b >= lowY, b <= highY]

pointStopsStepping:: Range -> Point -> Bool
pointStopsStepping ((x1,x2),(y1,y2)) point@(Point a1 b1 c1 d1) = 
  let Point a b c d = step point in 
  (a1 == a)&& (c==0) && ((a < x1) || (a > x2) || (b1 < y1))

pointPastTarget:: Range -> Point -> Bool
pointPastTarget ((_,maxX), (minY, _)) (Point a b c d) = 
  (a > maxX) || (( b < minY ) && (d < 0))

keepGoing:: Range -> Point -> Bool
keepGoing range point =     (not (pointInRange range point)) 
                        && (not (pointStopsStepping range point)) 
                        && (not (pointPastTarget range point))

throw range point = takeUntil (keepGoing range) $ iterate step point

maxHeight range point = let arc = throw range point  in 
  if (any (pointInRange range) arc) then Just (maximum $ map y arc) else Nothing

runVelocity range velX velY = throw range $ Point 0 0 velX velY

test range velX velY = maxHeight range (Point 0 0 velX velY)

findBest range =  maximumBy 
                (\(vel1, Just height1) (vel2, Just height2) -> 
                                            compare height1 height2) 
                $ filter (\(a, b)-> isJust b) 
                [((x,y), test range x y) | x <- [0.. 100], y <- [-10..1000]]

howManyShots range  = length 
                      $ filter (\(a, b)-> isJust b) 
                      [((x,y), test range x y) | x <- [0.. 500], y <- [-100..500]]

main = do
  let testRange = ((144,178), (-100,-76))
  let range = testRange
  print (findBest range)
  print (howManyShots range)