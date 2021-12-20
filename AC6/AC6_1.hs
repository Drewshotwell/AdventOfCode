import Data.List
import Data.List.Split

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    print $ length . last . take 81 . iterate nextDay . parseInintialDay $ inputStr

parseInintialDay :: String -> [Int]
parseInintialDay = map (\s -> read s :: Int) . splitOn ","

nextDay :: [Int] -> [Int]
nextDay [] = []
nextDay (x:xs) = if x > 0 then x - 1 : nextDay xs else 6 : 8 : nextDay xs