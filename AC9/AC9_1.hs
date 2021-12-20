import Data.List.Split

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    print $ riskLevel . lowPoints . parseHeightMap $ inputStr

parseHeightMap :: String -> [[((Int, Int), Int)]]
parseHeightMap str = 
    map (\(i,xs) ->
        map (\(j, x) -> 
            ((i, j), x)
        ) $ zip [0..] xs
    ) $ zip [0..] (map (map (\c -> readInt [c])) . splitOn "\n" $ str)

riskLevel :: [Int] -> Int
riskLevel = sum . map (+1)

lowPoints :: [[((Int, Int), Int)]] -> [Int]
lowPoints board = 
    map (snd . fst) . 
    filter (\((_, val), adVals) ->
        all (>val) adVals
    ) $
    zip (concat board) (map (\e -> adjacentVals e board) (concat board))

adjacentVals :: ((Int, Int), Int) -> [[((Int, Int), Int)]] -> [Int]
adjacentVals ((i, j), val) board =
    let colLen = length . head $ board
        rowLen = length board
        adjIdxs = 
            filter (\(t1, t2) ->
                idxInRange t1 0 (rowLen - 1) &&
                idxInRange t2 0 (colLen - 1)
            ) [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)]
    in  map (snd . \(i', j') -> board !! i' !! j') adjIdxs

readInt :: String -> Int
readInt s = read s :: Int

idxInRange :: Int -> Int -> Int -> Bool
idxInRange i lb ub = i >= lb && i <= ub