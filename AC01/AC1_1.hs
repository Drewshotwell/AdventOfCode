import Data.List
import Data.List.Split

main :: IO ()
main = do 
    inputStr <- readFile "input.txt"
    print $ numberOfIncreases . parseMeasurements $ inputStr

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