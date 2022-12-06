module Day03 () where
import Tools
import Data.Char (isLower, ord)
import Data.List (foldl1')

day03 input = do
    bags <- lines <$> input 
    print (p1 bags)
    print (p2 bags)


p1 :: [String] -> Int
p1 =  sum . map (prio . commonItem . splitHalf)

p2 :: [String] -> Int
p2 = sum . map (prio . commonItem) . reverseChunks 3


reverseChunks :: Int -> [a] -> [[a]]
reverseChunks n xs = reverseChunks' (xs,[])
    where
        reverseChunks' :: ([a],[[a]]) -> [[a]]
        reverseChunks' (ys,yss) = case splitAt n ys of 
            (as,[]) -> as:yss
            (as,bs) -> reverseChunks' (bs, as:yss)

splitHalf :: [a] -> [[a]]
splitHalf xs = as:[bs]
    where
        (as,bs) = splitAt n xs
        n = length xs `div` 2

commonItem :: [[Char]] -> Char
commonItem = head . groupIntersection

groupIntersection :: [[Char]] -> [Char]
groupIntersection = foldl1' pairIntersection

pairIntersection :: (Foldable t, Eq a) => t a -> [a] -> [a]
pairIntersection xs ys = filter (`elem` xs) ys


prio :: Char -> Int
prio item = 
    if isLower item then ord item - ord 'a' + 1
    else ord item - ord 'A' + 27

