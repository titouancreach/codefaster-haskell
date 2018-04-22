import System.Random (randomRIO)
import System.Timeout (timeout)
import System.Exit
import System.Posix.Signals
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data Response = Response Integer Integer

instance Show Response where
    show (Response x y) = "good: " ++ show x ++ " bad: " ++ show y

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



-- Pick a random line from a file
getRandomLine :: [String] -> IO String
getRandomLine xs = fmap (xs !! ) $ randomRIO (0, length xs - 10)

-- Let's say the line is valid if it is not empty
isLineValid :: String -> Bool
isLineValid "" = False
isLineValid _ = True

cleanLines :: [String] -> [String]
cleanLines = filter isLineValid . fmap trim

getInfiniteInput :: MVar Response -> [String] -> IO ()
getInfiniteInput mv total = do
    randomLine <- getRandomLine total
    print randomLine
    line <- getLine

    print $ line == randomLine
    t <- takeMVar mv
    if line == randomLine then
        putMVar mv $ addGoodAnswer t
    else
        putMVar mv $ addBadAnswer t

    --putMVar mv $ addGoodAnswer t
    print t
    getInfiniteInput mv total

main = do
    fileLines <- readFileLines "./codefaster.hs"
    mvar <- newMVar $ Response 0 0
    let totalLines = cleanLines fileLines
    timeout 50000000 $ getInfiniteInput mvar totalLines
    --randomLine <- getRandomLine fileLines
    --print randomLine

--main = readFileLines "./codefaster.hs" >>= return . cleanLines >>= getRandomLine

--main = do 
--    mvar <- newMVar 0
--    line <- timeout 5000000 $ getInfiniteInput mvar
--    t <- takeMVar mvar
--    print t
--    print line

