module Main where

import System.Random (randomRIO)
import System.Timeout (timeout)
import System.Exit
import System.Posix.Signals
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Char (isSpace)
import Debug.Trace

import Text.Regex.Base
import Text.Regex

import qualified Config as Config

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data Response = Response Integer Integer

instance Show Response where
    show (Response x y) = "Response(" ++ show x ++ ", " ++ show y ++ ")"

goodAnswer :: Response -> Integer
goodAnswer (Response x _) = x

badAnswer :: Response -> Integer
badAnswer (Response _ x) = x

anwsers :: Response -> Integer
anwsers (Response x y) = x + y

addGoodAnswer :: Response -> Response
addGoodAnswer (Response x y) = Response (x + 1) y

addBadAnswer :: Response -> Response
addBadAnswer (Response x y) = Response x $ y + 1


readFileLines :: FilePath -> IO [String]
readFileLines path = lines <$> readFile path


printResult :: Response -> IO ()
printResult (Response x y) = putStrLn $ "\nyou typed " ++ show x ++ " correct lines and " ++ show y ++ " incorrect lines"


-- Pick a random line from a file
getRandomLine :: [String] -> IO String
getRandomLine xs = fmap (xs !! ) $ randomRIO (0, length xs - 1)


improvedIgnored :: [Regex]
improvedIgnored = (mkRegex "^$") : Config.ignore 

checkIgnored :: [Regex] -> String -> Bool
checkIgnored regs s =  not $ any id $ map (\x -> matchTest x s) regs

cleanLines :: [String] -> [String]
cleanLines = filter (checkIgnored improvedIgnored) . fmap trim

getInfiniteInput :: MVar Response -> [String] -> IO ()
getInfiniteInput mv total = do
   
    randomLine <- getRandomLine total
    putStrLn randomLine
    line <- getLine

    t <- takeMVar mv
    if line == randomLine then
        putMVar mv $ addGoodAnswer t
    else
        putMVar mv $ addBadAnswer t

    getInfiniteInput mv total

toMicroSecond :: Int -> Int
toMicroSecond x = x * 1000000

main = do
    fileLines <- readFileLines "./codefaster.hs"
    mvar <- newMVar $ Response 0 0
    let totalLines = cleanLines fileLines
    timeout (toMicroSecond Config.timeout) $ getInfiniteInput mvar totalLines
    result <- takeMVar mvar
    printResult result
   
