{-# LANGUAGE DeriveFunctor #-}
module Tools where

import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.List (group, sort)
import Data.Char (digitToInt)
import Text.Megaparsec.Char (digitChar, space, string)
import Control.Arrow ((&&&))
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import Data.Map.Internal (Map)
import Data.Foldable (foldl')
import Data.Map.Internal ((!))
import Data.Set (Set)

run :: (IO String -> IO b) -> String -> IO b
run program filename = do 
  putStrLn "Test results = "
  program (getTest filename)
  putStrLn "Real results = "
  program (getInput filename)

runTest :: (IO String -> IO b) -> String -> IO b
runTest program filename = do 
  putStrLn "Test results = "
  program (getTest filename)

runReal :: (IO String -> IO b) -> String -> IO b
runReal program filename = do 
  putStrLn "Real results = "
  program (getInput filename)


getTest :: String -> IO String
getTest fname = readFile ("test/" ++ fname)

getInput :: String -> IO String
getInput fname = readFile ("input/" ++ fname)

getLines :: String -> IO [String]
getLines fname = lines <$> getInput fname

getParsedInput :: Parser a -> String -> IO a
getParsedInput p fname = myParse p <$> getInput fname

getParsedLines :: Parser a -> String -> IO [a]
getParsedLines p fname = map (myParse p) <$> getLines fname

myParse :: Parser a -> String -> a
myParse p s = case parse p "" s of
  Left e -> error (errorBundlePretty e)
  Right a -> a

type Parser = Parsec Void String

digit :: Parser Int
digit = digitToInt <$> digitChar

integer :: Parser Int
integer = L.decimal

signedInteger :: Parser Int
signedInteger = L.signed space integer

double :: Parser Double
double = L.float


-----------

counts :: Ord a => [a] -> [(a,Int)]
counts = map (head &&& length) . group . sort

repeatElems :: Ord a => [a] -> [a]
repeatElems = map head . filter ((>1) . length) . group .sort

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

fpow :: Int -> (a -> a) -> a -> a
fpow n f x = iterate f x !! n

data Point = Point {getX :: Int, getY :: Int} deriving (Eq, Show)
data Line = Line Point Point deriving (Eq, Show)

instance Ord Point where
  compare (Point x1 y1) (Point x2 y2) =
    if y1 == y2 then compare x1 x2
    else compare y1 y2

(!!!) :: [[a]] -> (Int,Int) -> a
mx !!! (x,y) = (mx !! y) !! x

data Map2D a = Map2D Point Point (Map Point a) deriving Functor 

instance Show a => Show (Map2D a) where
  show (Map2D (Point xMin yMin) (Point xMax yMax) pMap) = string 
    where
      string = unlines (map showLine [yMin..yMax])
      showLine y = concatMap (\x -> Map.findWithDefault " " (Point x y) pointToString) [xMin..xMax]
      pointToString = fmap show pMap

to2DMap :: [[a]] -> Map2D a
to2DMap arr2 = Map2D min max (Map.fromList pointsToElem)
  where
    min = Point 0 0
    max = Point (width - 1) (height - 1)
    
    pointsToElem = map (\p@(Point x y) -> (p,arr2 !!! (x,y))) points
    width = length (head arr2)
    height = length arr2
    points = [Point x y | x <- [0..width-1], y <- [0..height-1]]
