module Day04() where 

import Tools
import Text.Megaparsec.Char (char)
import Data.List.Split (splitOn)

data Range = Range Int Int

day04 input = do 
    pairs <- map parseRPair . lines <$> input 
    print (p1 pairs)
    print (p2 pairs)

p1 :: [(Range,Range)] -> Int
p1 = length . filter hasContain

p2 :: [(Range, Range)] -> Int
p2 = length . filter hasOverlap

overlaps :: Range -> Range -> Bool
overlaps (Range a1 a2) (Range b1 b2) =  not(a2 < b1 || a1 > b2)

hasOverlap :: (Range, Range) -> Bool
hasOverlap (r1,r2) = r1 `overlaps` r2

contains :: Range -> Range -> Bool
contains (Range a1 a2) (Range b1 b2) = a1 <= b1 && b2 <= a2

hasContain :: (Range, Range) -> Bool
hasContain (r1,r2) = r1 `contains` r2 || r2 `contains` r1

parseRPair :: String -> (Range, Range)
parseRPair s = (parseRange s1, parseRange s2)
    where s1:s2:_ = splitOn "," s

parseRange :: String -> Range
parseRange s = Range (read a1) (read a2)
    where a1:a2:_ = splitOn "-" s 




