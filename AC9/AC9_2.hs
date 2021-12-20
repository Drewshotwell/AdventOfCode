import Data.List
import Data.List.Split
import Data.Time

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    let board = parseHeightMap inputStr
    start <- getCurrentTime
    print $ basinProduct board
    stop <- getCurrentTime
    print $ diffUTCTime stop start

parseHeightMap :: String -> [[((Int, Int), Int)]]
parseHeightMap str = 
    map (\(i,xs) ->
        map (\(j, x) -> 
            ((i, j), x)
        ) $ zip [0..] xs
    ) $ zip [0..] (map (map (\c -> readInt [c])) . splitOn "\n" $ str)
        where readInt s = read s :: Int

basinProduct :: [[((Int, Int), Int)]] -> Int
basinProduct =
    product . take 3 . reverse . sort . map length . accumulateAdjElems . concatMap (filter (\e -> snd e /= 9))

accumulateAdjElems :: [((Int, Int), Int)] -> [[((Int, Int), Int)]]
accumulateAdjElems = 
    glueStickys (\((i1, j1), _) ((i2, j2), _) ->
        ((abs (i1 - i2) == 0) && (abs (j1 - j2) == 1)) ||
        ((abs (j1 - j2) == 0) && (abs (i1 - i2) == 1))
    )

glueStickys :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
glueStickys f = 
    foldl
        (\acc e ->
            let sticksForE = map (filter (f e)) acc -- [[a]]
                zipSticks = zip sticksForE acc -- [([a],[a])]
                hits = filter (\(se,_) -> not . null $ se) zipSticks
                unhits = filter (\(se,_) -> null se) zipSticks
                -- (unhits, hits) = span (\(se,_) -> null se) zipSticks
            in  if null . concat $ sticksForE 
                    then acc ++ [[e]]
                else
                    [e : (foldl1 union . map snd $ hits)] ++ (map snd unhits)
        ) []