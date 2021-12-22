import Data.List
import Data.List.Split

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    print $ optimalFuelCost . parseCrabPositions $ inputStr

parseCrabPositions :: String -> [Int]
parseCrabPositions = map (\s -> read s :: Int) . splitOn ","

optimalFuelCost :: [Int] -> Int
optimalFuelCost xs = minimum . map sum $ [map (\x -> abs (x - c)) xs | c <- [minimum xs..maximum xs]]