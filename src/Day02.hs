module Day02 (day02) where

import Tools
import Data.Char (ord)
import GHC.Char (chr)

day02 input = do
    pairs <- parsePairs <$> input 
    print (p1 pairs)
    print (p2 pairs)

p1 :: [(Char,Char)] -> Int
p1 = sum . map score

p2 :: [(Char, Char)] -> Int
p2 = sum . map (score . findMove)

findMove :: (Char, Char) -> (Char, Char)
findMove (m1, result) = (m1, m2)
    where 
        m2 = truncateMove (chr (ord m1 + 23 + diff))
        diff = case result of 
            'X' -> 2
            'Y' -> 0
            'Z' -> 1

truncateMove ch = 
    if ch > 'Z' then chr (ord ch - 3)
    else ch

parsePairs :: String -> [(Char, Char)]
parsePairs = map parsePair . lines 
    where 
        parsePair s = (s!!0, s!!2)

score :: (Char,Char) -> Int
score (m1,m2) = playScore (m1,m2) + moveScore m2

playScore :: (Char, Char) -> Int
playScore (m1,m2) = case diff of 
    1 -> 6
    0 -> 3
    2 -> 0
    where 
        diff = (ord m2 - ord m1 - 23) `mod` 3

moveScore :: Char -> Int
moveScore ch = case ch of 
    'X' -> 1
    'Y' -> 2
    'Z' -> 3


