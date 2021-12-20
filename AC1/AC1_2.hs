import Data.List
import Data.List.Split

main :: IO ()
main = do 
    inputStr <- readFile "input.txt"
    print $ numberOfIncreases . sumOfWindows . parseMeasurements $ inputStr

parseMeasurements :: String -> [Int]
parseMeasurements =
    map readInt .
    splitOn "\n"
        where readInt s = read s :: Int

numberOfIncreases :: [Int] -> Int
numberOfIncreases = length . filter (==1) . deltas . reverse

deltas :: [Int] -> [Int]
deltas [m] = []
deltas (m:ms) = let deltaM = if m > head ms then 1 else 0 in deltaM : deltas ms

measurementsWin :: [Int] -> [(Int, [Int])]
measurementsWin ms = zipWith (,) ms (windowIDList (length ms))

sumOfWindows :: [Int] -> [Int]
sumOfWindows ms = map (\n -> sum . map fst . filter (\m -> n `elem` snd m) $ (measurementsWin ms)) [1..length (measurementsWin ms) - 2]

windowIDList :: Int -> [[Int]]
windowIDList nLen = [1]:[1,2]:(triples \\ pLast) ++ [init (head pLast)] ++ [[head . last $ pLast]]
    where triples = map (\n -> [n, n + 1, n + 2]) [1..(nLen - 2)]
          pLast = drop (length triples - 2) triples