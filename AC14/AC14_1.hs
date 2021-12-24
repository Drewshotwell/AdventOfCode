import Data.List.Split
import Data.List
import qualified Data.Map as M

type PairingRules = M.Map String Char

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    let readInstructions = parseInstructions inputStr
    print $ leastFromMostDiff . fst . last . take 41 . iterate insertNextPairing $ readInstructions

-- Parse --

parseInstructions :: String -> (String, PairingRules)
parseInstructions =
    (\strDiv ->
        (strDiv !! 0, 
        M.fromList . map (\str -> let strTuple = splitOn " -> " str in (strTuple !! 0, head $ strTuple !! 1)) . splitOn "\n" $ strDiv !! 1)
    ) .
    splitOn "\n\n"

-- Debug --

debugInsts :: (String, PairingRules)
debugInsts = ("NNCB",M.fromList [("BB",'N'),("BC",'B'),("BH",'H'),("BN",'B'),("CB",'H'),("CC",'N'),("CH",'B'),("CN",'C'),("HB",'C'),("HC",'B'),("HH",'N'),("HN",'C'),("NB",'B'),("NC",'B'),("NH",'C'),("NN",'C')])

-- Process --

insertNextPairing :: (String, PairingRules) -> (String, PairingRules)
insertNextPairing (poly, rules) = -- (String, Map String Char)
    let slice begin end = take (end - begin) . drop begin
        insertedElems = map (rules M.!) [slice k (k + 2) poly | k <- [0..length poly - 2]]
    in  (concat . transpose $ [poly, insertedElems], rules)

leastFromMostDiff :: String -> Int
leastFromMostDiff poly = 
    let groupings = sortOn length . group . sort $ poly
    in  (length . last $ groupings) - (length . head $ groupings)