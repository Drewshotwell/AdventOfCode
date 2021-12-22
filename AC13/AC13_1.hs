import Data.List.Split
import Data.List

data Fold = XFold | YFold deriving (Show, Eq)
type FoldCmd = (Fold, Int)

main :: IO ()
main = do
   inputStr <- readFile "input_test.txt"
   let readInstructions = parseInstructions inputStr
   print readInstructions

-- Debug --

debugInstructions :: ([(Int, Int)], [FoldCmd])
debugInstructions = ([(6,10),(0,14),(9,10),(0,3),(10,4),(4,11),(6,0),(6,12),(4,1),(0,13),(10,12),(3,4),(3,0),(8,4),(1,10),(2,14),(8,10),(9,0)],[(YFold,7),(XFold,5)])

-- Parse --

parseInstructions :: String -> ([(Int, Int)], [FoldCmd])
parseInstructions = 
   (\strDiv -> (parseCoords (strDiv !! 0), parseFoldCmds (strDiv !! 1))) .
   splitOn "\n\n"

exStr = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0"

parseCoords :: String -> [(Int, Int)]
parseCoords =
   map (\str -> let strPair = splitOn "," str in (readInt $ strPair !! 0, readInt $ strPair !! 1)) .
   splitOn "\n"

parseFoldCmds :: String -> [FoldCmd]
parseFoldCmds =
   map (
      (\(axisChr:_:coordStr) -> 
         case axisChr of
            'x' -> (XFold, readInt coordStr)
            'y' -> (YFold, readInt coordStr)
      ) .
      drop 11
   ) .
   splitOn "\n"

readInt :: String -> Int
readInt s = read s :: Int

-- Process --

--foldPaper :: ([(Int, Int)], [FoldCmd]) -> (Int, Int)
foldPaper (crds, ((ft, line):rest)) =
   let foldingCrds =
         filter (\(i, j) ->
            case ft of
               XFold -> j < line
               YFold -> i < line
         ) crds
       remainingCrds = crds \\ foldingCrds
       foldedCrds = 
         map (\(i, j) ->
            case ft of
               XFold -> (i, j - line)
               YFold -> (i - line, j)
         ) foldingCrds
   in  remainingCrds