import Data.List
import Data.Char
import Data.Function
import Data.List.Split

data Segment = A | B | C | D | E | F | G deriving (Eq, Show, Enum, Read, Ord)

type SegNum = [Segment]

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    print $ occurencesOfUniqueNums $ parseNumReadings inputStr

occurencesOfUniqueNums :: [([SegNum], [SegNum])] -> Int
occurencesOfUniqueNums readings = 
    let uniqueLens = map (length . fst) uniqueNums
        fourDisplaysList = map (map length) . map snd $ readings
    in  foldl (
            \acc segLengths ->
                acc + (length . filter (\seg -> seg `elem` uniqueLens) $ segLengths)
        ) 0 fourDisplaysList

parseNumReadings :: String -> [([SegNum], [SegNum])]
parseNumReadings = 
    map ((\xs -> (parseSegTupleAt 0 xs, parseSegTupleAt 1 xs)) . splitOn " | ") .
    splitOn "\n"

parseSegTupleAt :: Int -> [String] -> [SegNum]
parseSegTupleAt i = map (readSegNum . map toUpper) . words . (!!i)

readSeg :: String -> Segment
readSeg s = read s :: Segment

readSegNum :: String -> SegNum
readSegNum = map (\c -> readSeg [c])

parseNumberFromSeg :: SegNum -> Maybe Int
parseNumberFromSeg segQuery = lookup segQuery numDefs

uniqueNums :: [([Segment], Int)]
uniqueNums = 
    concat . 
    filter (\xs -> length xs <= 1) . 
    groupBy ((==) `on` (length . fst)) . 
    sortBy (compare `on` (length . fst)) 
    $ numDefs

numDefs :: [(SegNum, Int)]
numDefs = [([A, B, C, E, F, G], 0),
           ([C, F], 1),
           ([A, C, D, E, G], 2),
           ([A, C, D, F, G], 3),
           ([B, C, D, F], 4),
           ([A, B, D, F, G], 5),
           ([A, B, D, E, F, G], 6),
           ([A, C, F], 7),
           ([A, B, C, D, E, F, G], 8),
           ([A, B, C, D, F, G], 9)]