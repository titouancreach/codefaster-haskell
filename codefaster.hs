
import System.Random (randomRIO)

readFileLines :: FilePath -> IO [String]
readFileLines path = lines <$> readFile path

getRandomLine :: [String] -> IO String
getRandomLine xs = fmap (xs !! ) $ randomRIO (0, length xs - 10)



main = readFileLines "./codefaster.hs" >>= getRandomLine >>= (\x -> putStrLn x)  