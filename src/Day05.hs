{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Day05 () where
import Tools
import  qualified Data.Map.Internal as M
import Data.IntMap.Lazy (insert)
import Data.Foldable (foldl')
import Data.List (transpose)
import Data.Char (isDigit, isAlpha, digitToInt)
import Text.Megaparsec.Char (space, string)
import Data.List.Split (splitOn)

data Move = Move Int Int Int
type Stack = [Char]
type Supplies = M.Map Int Stack

day05 input = do 
    (sup,mov) <- parseInput <$> input 
    print (p1 (sup,mov))
    print (p2 (sup,mov))

p1 :: (Supplies, [Move]) -> String
p1 (supplies, moves) = getMessage (moveList1 moves supplies) 

p2 :: (Supplies, [Move]) -> String
p2 (supplies, moves) = getMessage (moveList2 moves supplies) 


getMessage :: Supplies -> String
getMessage = map (head . snd) . M.toList

parseInput :: String -> (Supplies, [Move])
parseInput s = (parseSupplies supplies, parseMoves moves)
    where 
        supplies:moves:_ = splitOn "\n\n" s 

parseSupplies :: String -> Supplies
parseSupplies s = (M.fromList . map parseStack . filter hasInfo) s'
    where
        hasInfo line = isDigit (last line)  
        s' = transpose (lines s)

parseMoves :: String -> [Move]
parseMoves = map (myParse moveParser) . lines         

parseStack :: String -> (Int, Stack)
parseStack s = (digitToInt (last s), filter isAlpha s)     

moveParser :: Parser Move
moveParser = do 
    string "move "
    num <- integer
    string " from " 
    from <- integer
    string " to "
    to <- integer
    return (Move num from to)

moveList1 :: [Move] -> Supplies -> Supplies
moveList1 moves supplies = foldl' (flip move1) supplies moves

moveList2 :: [Move] -> Supplies -> Supplies
moveList2 moves supplies = foldl' (flip move2) supplies moves

move1 :: Move -> Supplies -> Supplies
move1 (Move num from to) supplies = ((M.insert from fromStack') . (M.insert to toStack')) supplies
    where
        fromStack' = drop num fromStack
        toStack' = reverse (take num fromStack) ++ toStack
        fromStack = supplies M.! from
        toStack = supplies M.! to

move2 :: Move -> Supplies -> Supplies
move2 (Move num from to) supplies = ((M.insert from fromStack') . (M.insert to toStack')) supplies
    where
        fromStack' = drop num fromStack
        toStack' = take num fromStack ++ toStack
        fromStack = supplies M.! from
        toStack = supplies M.! to
