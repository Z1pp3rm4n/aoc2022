module Day08 () where

import Tools
import qualified Data.Map.Internal as M
import Data.Char (digitToInt)

testForest =  (fmap digitToInt) . fromArr2D . lines <$> (getTest "08")

day08 :: IO String -> IO ()
day08 input = do 
    forest <- (fmap digitToInt) . fromArr2D . lines <$> input
    print (p1 forest)
    print (p2 forest)

p1 :: Map2D Int -> Int
p1 forest@(Map2D _ _ pMap) = length (filter (`isVisible` forest) ps)
    where
        ps = map fst (M.toList pMap)

p2 :: Map2D Int -> Int
p2 forest = maximum (map (`scenicScore` forest) ps) 
    where 
        ps = getPoints forest

scenicScore :: Point -> Map2D Int -> Int
scenicScore p forest = product (map (\dir -> viewDistance dir p forest) dirs)

viewDistance :: (Int, Int) -> Point -> Map2D Int -> Int
viewDistance dir p forest = 
    if (null remTrees) then length shorterTrees
    else (length shorterTrees) + 1
    where
        (shorterTrees, remTrees) =  span (< treeHeight) otherHeights
        otherHeights = map (`getHeight` forest) pointsInDir
        treeHeight = getHeight p forest
        pointsInDir = getPointsInDir dir p forest



isVisible :: Point -> Map2D Int -> Bool
isVisible p forest = any (\dir -> isVisibleFromDir dir p forest) dirs

isVisibleFromDir :: (Int, Int) -> Point -> Map2D Int -> Bool
isVisibleFromDir dir p forest = all (< treeHeight) otherHeights
    where
        otherHeights = map (`getHeight` forest) pointsInDir
        treeHeight = getHeight p forest
        pointsInDir = getPointsInDir dir p forest

getHeight :: Point -> Map2D Int -> Int
getHeight p (Map2D _ _ pMap) = pMap M.! p

getPointsInDir :: (Int, Int) -> Point -> Map2D a -> [Point]
getPointsInDir (xStep, yStep) (Point x y) (Map2D (Point xMin yMin) (Point xMax yMax) _ ) =
    tail (zipWith Point xs ys)
    where
        xs = if (xStep >= 0) then [x, x + xStep .. xMax]
            else [x, x + xStep .. xMin]
        ys = if (yStep >= 0) then [y, y + yStep .. yMax]
            else [y, y + yStep .. yMin]

getPoints :: Map2D a -> [Point]
getPoints (Map2D _ _ pMap) = map fst (M.toList pMap)

dirs = [(0,1),(0,-1),(1,0),(-1,0)]

