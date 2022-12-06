{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day06 () where

import Tools
import Data.List (sort)
import GHC.OldList (group)
import Data.IntMap (size)

day06 input = do 
    inp <- input
    print (p1 inp)
    print (p2 inp)

p1 :: String -> Int
p1 = marker 4

p2 :: String -> Int
p2 = marker 14

noRepeatElem :: Ord a => [a] -> Bool
noRepeatElem =  all ((==1) . length) . group . sort

marker :: Int -> String -> Int
marker size = marker' size size

marker' :: Int -> Int -> String -> Int
marker' cnt size xs =
    if noRepeatElem (take size xs) then cnt
    else marker' (cnt + 1) size (tail xs)

