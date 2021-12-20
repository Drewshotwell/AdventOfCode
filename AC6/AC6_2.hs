import Data.List
import Data.List.Split

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    print $ sum . last . take 257 . iterate nextDay . instancesOfNums . parseInintialDay $ inputStr

parseInintialDay :: String -> [Int]
parseInintialDay = map (\s -> read s :: Int) . splitOn ","

instancesOfNums :: [Int] -> [Int]
instancesOfNums xs = [length . filter (==n) $ xs | n <- [0..8]]

nextDay :: [Int] -> [Int]
nextDay day =
    let dayTail = tail day
    in  (take 6 dayTail) ++ [(dayTail !! 6) + head day] ++ (drop 7 dayTail) ++ [head day]