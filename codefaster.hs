import System.Random (randomRIO)
import System.Timeout (timeout)
import System.Exit
import System.Posix.Signals
import Control.Concurrent
import Control.Concurrent.MVar



readFileLines :: FilePath -> IO [String]
readFileLines path = lines <$> readFile path

-- Pick a random line from a file
getRandomLine :: [String] -> IO String
getRandomLine xs = fmap (xs !! ) $ randomRIO (0, length xs - 10)

-- Let's say the line is valid if it is not empty
isLineValid :: String -> Bool
isLineValid "" = False
isLineValid _ = True

-- This would probably infinite loop if no lines are valid
getValidRandomLine :: [String] -> IO String
getValidRandomLine xs = getRandomLine xs >>= (\line -> if isLineValid line then return line else getValidRandomLine xs)

-- Do notation version
-- getValidRandomLine xs = do 
--    line <- getRandomLine xs
--    if isLineValid line then return line else getValidRandomLine xs  

goInfinitWithAcc :: MVar Integer -> IO ()
goInfinitWithAcc mv = do
    line <- getLine
    print line
    t <- takeMVar mv
    putMVar mv $ t + 1
    print t
    goInfinitWithAcc mv


getInfiniteInput :: MVar Integer -> IO ()
getInfiniteInput mvar = goInfinitWithAcc mvar

-- main = readFileLines "./codefaster.hs" >>= getValidRandomLine >>= putStrLn
main = do 
    mvar <- newMVar 0
    line <- timeout 5000000 $ getInfiniteInput mvar
    t <- takeMVar mvar
    print t
    print line

