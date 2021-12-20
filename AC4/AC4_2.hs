import Data.List.Split
import Data.Maybe
import Data.Function
import Data.List

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    let numInputs = parseNumInputs . head . words $ inputStr -- [Int]
    let bingoSheets = parseBingoSheets . tail . words $ inputStr -- [[Int]]
    print $ gameScore bingoSheets numInputs

parseNumInputs :: String -> [Int]
parseNumInputs = map (\dStr -> read dStr :: Int) . splitOn ","

parseBingoSheets :: [String] -> [[Int]]
parseBingoSheets [] = []
parseBingoSheets bingoElems =
    let curSheetL = take 25 bingoElems
        remainingNums = drop 25 bingoElems
        curSheetLInt = map (\dStr -> read dStr :: Int) curSheetL
    in  curSheetLInt : parseBingoSheets remainingNums
    
markedNums :: [Int] -> [Int] -> [Int]
markedNums sheet idxs = [sheet !! i | i <- idxs]

unMarkedNums :: [Int] -> [Int] -> [Int]
unMarkedNums sheet idxs = markedNums sheet ([0..24] \\ idxs)

gameScore :: [[Int]] -> [Int] -> Int
gameScore sheets nums =
    let processedSheets = [(s, winningDepthForSheet s nums) | s <- sheets]
        pElem = length . snd . snd
        lastBingoSheet = maximumBy (compare `on` pElem) processedSheets -- indexes, last elems
    in  (head . snd . snd $ lastBingoSheet) * (sum $ unMarkedNums (fst lastBingoSheet) (fst (snd lastBingoSheet)))

bingoSheetWon :: [Int] -> Bool
bingoSheetWon idxs = 
    let idxsMod = map (\n -> n `mod` 5) idxs
    in  any id (map (\n -> (replicate 5 n) `isSubsequenceOf` idxsMod) [0..4]) ||
        any id (map (\is -> is `isSubsequenceOf` (sort idxs)) [take 5 [k..] | k <- [0,5..20]])

winningDepthForSheet :: [Int] -> [Int] -> ([Int], [Int])
winningDepthForSheet sheet =
    fromJust . find (\p -> bingoSheetWon (fst p)) .
    scanl (\acc n -> 
              (fst acc ++ (markedIdxsForSheet sheet n), n:(snd acc))
          ) ([], [])

markedIndexes :: [[Int]] -> Int -> [[Int]]
markedIndexes sheets num = foldl (\acc s -> acc ++ [markedIdxsForSheet s num]) [] sheets

markedIdxsForSheet :: [Int] -> Int -> [Int]
markedIdxsForSheet sheet num = filter (\i -> sheet !! i == num) [0..length sheet - 1]