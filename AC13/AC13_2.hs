import Data.List.Split
import Data.List

data Fold = VFold | HFold deriving (Show, Eq)
type FoldCmd = (Fold, Int)
type Vector = (Int, Int)
type Matrix = (Vector, Vector)

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    let readInstructions = parseInstructions inputStr
    print $ map (\v -> v `multiplyByMatrix` xAxisReflection) . foldPaper $ readInstructions
    -- Code found with the power of Desmos

-- Debug --

debugInstructions :: ([Vector], [FoldCmd])
debugInstructions = ([(6,10),(0,14),(9,10),(0,3),(10,4),(4,11),(6,0),(6,12),(4,1),(0,13),(10,12),(3,4),(3,0),(8,4),(1,10),(2,14),(8,10),(9,0)],[(HFold,7),(VFold,5)])

-- Parse --

parseInstructions :: String -> ([Vector], [FoldCmd])
parseInstructions = 
    (\strDiv -> (parseCoords (strDiv !! 0), parseFoldCmds (strDiv !! 1))) .
    splitOn "\n\n"

parseCoords :: String -> [Vector]
parseCoords =
    map (\str -> let strPair = splitOn "," str in (readInt $ strPair !! 0, readInt $ strPair !! 1)) .
    splitOn "\n"

parseFoldCmds :: String -> [FoldCmd]
parseFoldCmds =
    map (
        (\(axisChr:_:coordStr) -> 
            case axisChr of
                'x' -> (VFold, readInt coordStr)
                'y' -> (HFold, readInt coordStr)
        ) .
        drop 11
    ) .
    splitOn "\n"

readInt :: String -> Int
readInt s = read s :: Int

-- Process --

yAxisReflection :: Matrix
yAxisReflection = (((-1), 0), (0, 1))

xAxisReflection :: Matrix
xAxisReflection = ((1, 0), (0, (-1)))

multiplyByMatrix :: Vector -> Matrix -> Vector
multiplyByMatrix (x, y) ((m00, m01), (m10, m11)) = (m00 * x + m10 * y, m01 * x + m11 * y)

translateByVector :: Vector -> Vector -> Vector
translateByVector (x, y) (a, b) = (x + a, y + b)

foldPaper :: ([Vector], [FoldCmd]) -> [Vector]
foldPaper (crds, []) = crds
foldPaper (crds, ((ft, line):rest)) =
    let foldingCrds =
            filter (\(i, j) ->
                case ft of
                    VFold -> i > line
                    HFold -> j > line
            ) crds
        remainingCrds = crds \\ foldingCrds
        foldedCrds = 
            map (\v ->
                case ft of
                    VFold -> ((v `translateByVector` (-line, 0)) `multiplyByMatrix` yAxisReflection) `translateByVector` (line, 0)
                    HFold -> ((v `translateByVector` (0, -line)) `multiplyByMatrix` xAxisReflection) `translateByVector` (0, line)
            ) foldingCrds
    in  foldPaper (nub $ foldedCrds ++ remainingCrds, rest)