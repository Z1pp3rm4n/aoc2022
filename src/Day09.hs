{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day09 () where

import Tools
import Data.Foldable (Foldable(foldl'))
import qualified Data.List as L (last, replicate)
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Data.Char (intToDigit)

type Rope = [Point]
type Dir = String 
data Motion = Motion Dir Int

day09 input = do 
    motions <- parseInput <$> input 
    print (p1 motions)
    print (p2 motions)
    
visualise (rope, hist) = toString2DMap (fromPointMap pMap)
    where 
        pMap = foldl' (flip (uncurry M.insert)) historyMap numberedRope
        historyMap = foldl' (flip (\p -> M.insert p '#')) (M.singleton (Point 0 0) 's') hist
        numberedRope = zip rope ['0'..'9']


p1 ms = countUniq hist
    where 
        (_, hist) = foldl' (flip moveRope) start ms
        start = (startRope, [Point 0 0])
        startRope = L.replicate 2 (Point 0 0)

p2 ms = countUniq hist
    where 
        (_, hist) = foldl' (flip moveRope) start ms
        start = (startRope, [Point 0 0])
        startRope = L.replicate 10 (Point 0 0)


moveRope :: Motion -> (Rope, [Point]) -> (Rope, [Point])
moveRope (Motion dir n) = fpow n (moveRopeInDir dir)

moveRopeInDir :: Dir -> (Rope, [Point]) -> (Rope, [Point])
moveRopeInDir dir (head:tail, hist) = (newHead:newTail, newLast:hist)
    where
        newLast = L.last newTail
        newHead = moveHead dir head
        newTail = ropeFollow newHead tail

ropeFollow :: Point -> [Point] -> [Point]
ropeFollow front [] = []
ropeFollow front (next:tail) = newNext : ropeFollow newNext tail
    where newNext = knotFollow front next


moveHead :: Dir -> Point -> Point
moveHead "U" (Point x y) = Point x (y - 1)
moveHead "D" (Point x y) = Point x (y + 1)
moveHead "L" (Point x y) = Point (x - 1) y 
moveHead "R" (Point x y) = Point (x + 1) y

knotFollow :: Point -> Point -> Point
knotFollow (Point xFront yFront) (Point xNext yNext)
    | distSq <= 2 = Point xNext yNext
    | otherwise   = Point (xNext + xStep) (yNext + yStep)
    where
        xStep = signum xDiff
        yStep = signum yDiff
        xDiff = xFront - xNext
        yDiff = yFront - yNext
        distSq = xDiff^2 + yDiff^2

parseInput :: String -> [Motion]
parseInput = map parseMotion . lines

parseMotion :: String -> Motion
parseMotion s = Motion dir (read n)
    where 
        (dir:n:_) = words s