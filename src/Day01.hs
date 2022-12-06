module Day01 () where


import Tools
import Data.List.Split (split)
import Data.List.Split.Internals (splitOn) 
import Data.List (sort, sortBy)

day01 :: IO String -> IO()
day01 input = do 
    calories <- caloriesPerElf <$> input
    print (p1 calories)
    print (p2 calories)

p1 :: [Int] -> Int
p1 = maximum 

p2 :: [Int] -> Int
p2 = sum . take 3 . descSort   
    where 
        descSort = sortBy (flip compare) 

caloriesPerElf :: String -> [Int]
caloriesPerElf = map sum . snacksPerElf

snacksPerElf :: String -> [[Int]]
snacksPerElf =  map (map read . lines) . splitOn "\n\n"

