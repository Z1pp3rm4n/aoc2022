module Day10 () where

import Tools
import Text.Megaparsec.Char (string)
import Control.Applicative ((<|>))
import qualified Data.Map as M
import Data.Foldable (foldl')

data Cmd = ADDX Int | NOOP 
type History = M.Map Int Int
type CPU = (Int, History, Int)
type Screen = M.Map Point Char

day10 input = do 
    cmds <- map (myParse cmdParser) . lines <$> input
    let (_, hist, _) = executeCmds start cmds
    print (p1 hist)
    putStrLn (p2 hist)

p1 :: History -> Int
p1 hist = sum . map (`signalStrength` hist) $ cycles
    where
        cycles = [20,60,100,140,180,220] 

p2 :: History -> String
p2 hist = toString2DMap map2d
    where
        map2d = Map2D min max screen
        min = Point 0 0
        max = Point (width - 1) (height - 1) 
        screen = draw hist

draw :: History -> Screen 
draw hist = screen
    where
        (screen, _) = foldl' drawCycle (emptyScreen, hist) cycles
        emptyScreen = M.empty
        cycles = [1..240]

drawCycle :: (Screen, History) -> Int -> (Screen, History)
drawCycle (screen, hist) cycle = 
    if overlap then (M.insert cursor '#' screen, hist)
    else (screen, hist)
    where
        overlap = abs (xCpu - xCursor) <= 1 
        cursor = Point xCursor yCursor
        xCursor = (cycle - 1) `mod` width
        yCursor = (cycle - 1) `div` width
        xCpu = xValue cycle hist


width = 40 
height = 6


start = (1, M.singleton 1 1, 1)

signalStrength :: Int -> History -> Int
signalStrength cycle hist  = (xValue cycle hist) * cycle --TODO

xValue :: Int -> History -> Int
xValue n hist = 
    if n `M.member` hist then hist M.! n 
    else xValue (n - 1) hist

executeCmds :: CPU -> [Cmd] -> CPU
executeCmds = foldl' executeCmd

executeCmd :: CPU -> Cmd -> CPU
executeCmd (x, hist, cycle) NOOP = (x, hist, cycle + 1)
executeCmd (x, hist, cycle) (ADDX v) = (x + v, newHist, cycle + 2)
    where 
        newHist = M.insert (cycle + 2) (x + v) hist

cmdParser :: Parser Cmd
cmdParser = addxParser <|> noopParser 
    where 
        addxParser = do 
            string "addx "
            n <- signedInteger
            return $ ADDX n
        noopParser = NOOP <$ string "noop"
        