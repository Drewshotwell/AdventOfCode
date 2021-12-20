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
    let aims = tail $ scanl (\acc c -> 
                        case fst c of Down    -> acc + snd c 
                                      Up      -> acc - snd c
                                      Forward -> acc) 0 commandList
        depth = foldl (\acc p -> acc + fst p * snd (snd p)) 0 . filter (\p -> fst (snd p) == Forward) $ zip aims commandList
        horPos = sum . map snd . filter (\c -> fst c == Forward) $ commandList
    in  depth * horPos