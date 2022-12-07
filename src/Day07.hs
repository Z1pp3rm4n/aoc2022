module Day07() where

import Tools
import Text.Megaparsec.Char ( alphaNumChar, string, space, char, letterChar )
import qualified Data.Map.Internal as M
import Control.Applicative ((<|>))
import Text.Megaparsec (some)
import Data.IntMap (size)
import Data.Foldable (Foldable(foldl'))
import Data.List (sort)

data Item = File String Int | Folder String ItemMap deriving Show
type ItemMap = M.Map String Item
type Context = [(String, ItemMap)]

type Zipper = (Item, Context)

data Message = Cd String | Up | Ls | Info Item deriving Show

day07 input = do 
    (root, _) <- upmost . buildTree <$> input
    let folderSizes = snd (findSizes root)
    print (p1 folderSizes)
    print (p2 folderSizes)

p1 :: [Int] -> Int
p1 = sum . filter (<= 100000)

p2 :: [Int] -> Int
p2 sizes = (head . dropWhile (< minDelSize) . sort ) sizes
    where 
        minDelSize = 30000000 - (70000000 - rootSize)
        rootSize = head sizes 


findSizes :: Item -> (Int, [Int])
findSizes (Folder _ items) = (size, size:cache)
    where
        size = sum (map fst results)
        cache = concatMap snd results
        results = map findSizes itemList 
        itemList = map snd (M.toList items )
findSizes (File _ size) = (size, [])

buildTree :: String -> Zipper 
buildTree s = foldl' (flip interpret) start messages
    where       
        messages = (map (myParse messageParser) . tail . lines) s
        start = (root, []) 
        root = Folder "/" M.empty
        


interpret :: Message -> Zipper -> Zipper
interpret (Cd name) = open name
interpret Up = up
interpret Ls = id
interpret (Info item) = add item

open :: String -> Zipper -> Zipper
open name (Folder pName pItems, ctx) = (newFolder, (pName, siblings):ctx)
    where
        newFolder = pItems M.! name
        siblings = M.delete name pItems

up :: Zipper -> Zipper
up (folder, (pName, siblings):ctx) = (parent, ctx)
    where
        (Folder name _) = folder
        parent = Folder pName (M.insert name folder siblings)
up (folder, []) = (folder, [])

upmost :: Zipper -> Zipper
upmost (folder, []) = (folder, [])
upmost zipper = upmost (up zipper)

add :: Item -> Zipper -> Zipper
add item (Folder pName pItems, ctx) = (newFolder, ctx)
    where
        newFolder = Folder pName newItems
        newItems = M.insert name item pItems
        name = getName item

getName :: Item -> String
getName (File name _) = name
getName (Folder name _) = name



messageParser :: Parser Message
messageParser = upParser <|> cdParser <|> lsParser <|> infoParser
    where
        upParser = do 
            string "$ cd .."
            return Up
        cdParser = do
            string "$ cd "
            name <- nameParser
            return $ Cd name
        lsParser = do
            string "$ ls"
            return Ls
        infoParser = Info <$> (folderParser <|> fileParser)

folderParser :: Parser Item
folderParser = do
    string "dir "
    name <- nameParser
    return $ Folder name M.empty

fileParser :: Parser Item
fileParser = do
    size <- integer
    space
    name <- nameParser
    return $ File name size


nameParser :: Parser String
nameParser = some (letterChar <|> char '.')




