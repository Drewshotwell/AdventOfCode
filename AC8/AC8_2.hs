import Data.List
import Data.Char
import Data.Maybe
import Data.Function
import Data.List.Split

data Segment = A | B | C | D | E | F | G deriving (Eq, Enum, Read, Ord)

type SegNum = [Segment]

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    let readings = parseNumReadings inputStr -- [([SegNum], [SegNum])
    print $ sum . map (digitsToNum . map segNumToInt . correctNumOutputs) $ readings

-------------

-- Parsing --

parseNumReadings :: String -> [([SegNum], [SegNum])]
parseNumReadings = 
    map ((\xs -> (parseSegTupleAt 0 xs, parseSegTupleAt 1 xs)) . splitOn " | ") .
    splitOn "\n"
        where parseSegTupleAt i = map (readSegNum . map toUpper) . words . (!!i)

readSeg :: String -> Segment
readSeg s = read s :: Segment

readSegNum :: String -> SegNum
readSegNum = map (\c -> readSeg [c])

-- Data Transformation --

incorrectNumSeqs :: [SegNum] -> [(SegNum, Int)]
incorrectNumSeqs = flip zip (map snd numDefs) . map sort . sortBy (compare `on` length)

segNumToInt :: SegNum -> Int
segNumToInt s = fromJust $ lookup s numDefs

-- Utility --

countOccurences :: Eq a => [a] -> a -> Int
countOccurences [] _ = 0
countOccurences ys x = length [xs | xs <- ys, xs == x]

digitsToNum :: [Int] -> Int
digitsToNum numL = (read . concatMap show $ numL) :: Int

-- Correction Mappings --

correctNumOutputs :: ([SegNum], [SegNum]) -> [SegNum]
correctNumOutputs (enumTestings, numOutputsIn) =
    let cm = correctionMappingFor . incorrectNumSeqs $ enumTestings
    in  map (correctNumOutput cm) numOutputsIn

correctNumOutput :: [(Segment, Segment)] -> SegNum -> SegNum
correctNumOutput cm = sort . map (\s -> fromJust . lookup s $ cm)

correctionMappingFor :: [(SegNum, Int)] -> [(Segment, Segment)]
correctionMappingFor wrongMappings =
    let readingFor k = (map fst . sortBy (compare `on` snd) $ wrongMappings) !! k -- SegNum i.e. [Segment]

        occurencesOfSegsIn xs = zip (map (countOccurences (concatMap fst wrongMappings)) xs) xs

        mappingToF = snd . fromJust . find (\x -> fst x == 9) $ occurencesOfSegsIn [(A)..(G)]
        mappingToC = head $ (readingFor 1) \\ [mappingToF]
        mappingToA = head $ (readingFor 7) \\ (readingFor 1)

        mB'mD = (readingFor 4) \\ (readingFor 7)
        mappingToB = snd . fromJust . find (\x -> fst x == 6) $ occurencesOfSegsIn mB'mD
        mappingToD = head $ mB'mD \\ [mappingToB]

        mappingToE = snd . fromJust . find (\x -> fst x == 4) $ occurencesOfSegsIn [(A)..(G)]

        occursSeven = map snd . filter (\x -> fst x == 7) $ occurencesOfSegsIn [(A)..(G)]
        mappingToG = head $ occursSeven \\ [mappingToD]

    in  [(mappingToC, C), (mappingToF, F), (mappingToA, A), (mappingToB, B), (mappingToD, D), (mappingToE, E), (mappingToG, G)]

numDefs :: [(SegNum, Int)]
numDefs =
    [([C, F], 1),
    ([A, C, F], 7),
    ([B, C, D, F], 4),
    ([A, C, D, E, G], 2),
    ([A, C, D, F, G], 3),
    ([A, B, D, F, G], 5),
    ([A, B, C, E, F, G], 0),
    ([A, B, D, E, F, G], 6),
    ([A, B, C, D, F, G], 9),
    ([A, B, C, D, E, F, G], 8)]