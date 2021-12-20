import Data.List

data Command = Forward | Up | Down deriving (Eq, Show)

main :: IO ()
main = do
    commandStrs <- readFile "./input.txt"
    print $
        multiplyDirs . parseCommands . words $ commandStrs

parseCommands :: [String] -> [(Command, Int)]
parseCommands [] = []
parseCommands (str1:str2:strs) = 
    let command = case str1 of "forward" -> Forward
                               "down"    -> Down
                               "up"      -> Up
        value   = read str2 :: Int
    in  (command, value) : parseCommands strs

multiplyDirs :: [(Command, Int)] -> Int
multiplyDirs commandList = 
    let depth = foldl (\acc c -> if fst c == Down then acc + snd c else acc - snd c) 0 $ filter (\c -> fst c /= Forward) commandList
        horPos = sum . map snd . filter (\c -> fst c == Forward) $ commandList
    in  depth * horPos
