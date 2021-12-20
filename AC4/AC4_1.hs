import Data.List.Split
import Data.Maybe
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

--gameScore :: [[Int]] -> [Int] -> Int
gameScore sheets nums =
    let processedSheets = unmarkedWinningNums sheets nums -- ([Int], Int), unmarked nums of winning list and the last marked elem
        unMarkedSum = sum . fst $ processedSheets
        lastElem = snd processedSheets
    in  unMarkedSum * lastElem

unmarkedWinningNums :: [[Int]] -> [Int] -> ([Int], Int) -- should return the unmarked numbers of the winnig sheet and the last number marked 
unmarkedWinningNums sheets = 
    (\p -> 
        (unMarkedNums 
            (sheets !! 
                (fst $ snd $ bingoSheetWon (fst p))
            )
            (snd $ snd $ bingoSheetWon (fst p)) 
        , snd p) ) . -- p is a ([[Int]], Int), this function produces [Int]
    fromJust . find (\p -> fst (bingoSheetWon (fst p))) . -- produces ([[Int]], Int), the bingo state of indexes for each sheet and last number in which one sheet has won
    -- scan produces a [([[Int]], Int)], list of indexes marked for each sheet and the last number marked
    scanl (\acc n ->
            (zipWith (++) (fst acc) (markedIndexes sheets n), n)
          ) (((take (length $ head sheets)) $ repeat []), 0) 
          -- $ [number list]

bingoSheetWon :: [[Int]] -> (Bool, (Int, [Int]))
-- takes in list of indexes for each sheet and returns whether a sheet has won and, if one has, a list of indexes and the index of that winning sheet
bingoSheetWon indexList = 
    fromMaybe (False, (0,[])) .
    find fst .
    -- scan produces a [(Bool, (Int, [Int]))] list of Booleans for each index state and the index and indexes of each index list
    scanl (\acc idxs -> 
            let idxsMod = map (\n -> n `mod` 5) idxs
            in  (fst acc || ((any id (map (\n -> (replicate 5 n) `isSubsequenceOf` idxsMod) [0..4]) ||
                              any id (map (\is -> is `isSubsequenceOf` (sort idxs)) [take 5 [k..] | k <- [0,5..20]])))
                , (fromJust . elemIndex idxs $ indexList, idxs))
          ) (False, (0,[]))
    $ indexList

markedIndexes :: [[Int]] -> Int -> [[Int]]
markedIndexes sheets num = foldl (\acc s -> acc ++ [markedIdxsForSheet s num]) [] sheets

markedIdxsForSheet :: [Int] -> Int -> [Int]
markedIdxsForSheet sheet num = filter (\i -> sheet !! i == num) [0..length sheet - 1]