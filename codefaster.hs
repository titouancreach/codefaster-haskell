import System.Random (randomRIO)

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


main = readFileLines "./codefaster.hs" >>= getValidRandomLine >>= putStrLn