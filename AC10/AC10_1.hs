import Data.List.Split
import Data.List
import Data.Maybe
import Data.Function

data BracketType = 
    LPar | RPar |
    LSqr | RSqr |
    LCur | RCur |
    LArr | RArr | 
    BNull deriving (Show, Eq)

type Bracket = (BracketType, Int)

data PairingState = 
    PairingState { 
        leftB :: Bracket,
        rightB :: Bracket,
        message :: String
    } deriving (Show, Eq)

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    print $ syntaxErrScore . parseSyntax $ inputStr

-- Parse --

parseSyntax :: String -> [[Bracket]]
parseSyntax = 
    map parseTokens .
    splitOn "\n"

parseTokens :: String -> [Bracket]
parseTokens = 
    fst .
    foldl (\(acc, lastDepth) c -> -- acc is [(BracketType, Int)]
        case c of
            '(' -> (acc ++ [(LPar, lastDepth + 1)], lastDepth + 1)
            '[' -> (acc ++ [(LSqr, lastDepth + 1)], lastDepth + 1)
            '{' -> (acc ++ [(LCur, lastDepth + 1)], lastDepth + 1)
            '<' -> (acc ++ [(LArr, lastDepth + 1)], lastDepth + 1)
            
            ')' -> (acc ++ [(RPar, lastDepth)], lastDepth - 1)
            ']' -> (acc ++ [(RSqr, lastDepth)], lastDepth - 1)
            '}' -> (acc ++ [(RCur, lastDepth)], lastDepth - 1)
            '>' -> (acc ++ [(RArr, lastDepth)], lastDepth - 1)
    ) ([(BNull, 0)], 0) -- list of (BracketType, Int) i.e. Bracket

-- Debug --

exCorrupt :: [Bracket]
exCorrupt = parseTokens "{([(<{}[<>[]}>{[]{[(<()>"

exNonCorrupt :: [Bracket]
exNonCorrupt = parseTokens "[({(<(())[]>[[{[]{<()<>>"

-- Process sequences --

illegalCharacterIn :: [Bracket] -> Maybe PairingState
illegalCharacterIn = find (\ps -> (take 3 $ message ps) == "Err") . processTokens . tail

syntaxErrScore :: [[Bracket]] -> Int
syntaxErrScore bss =
    let illegalBts = map (fst . rightB . fromJust) . filter (/=Nothing) . map illegalCharacterIn $ bss
    in  sum
        [case bt of
            RPar -> 3
            RSqr -> 57
            RCur -> 1197
            RArr -> 25137
        | bt <- illegalBts]

processTokens :: [Bracket] -> [PairingState]
processTokens = 
    foldl (\acc b@(bt, d) -> 
        if bt `elem` [LPar, LSqr, LCur, LArr] then
            acc ++ [(PairingState b (BNull, d) (show bt ++ " hanging"))]
        else
            let oldState =
                    fromMaybe 
                    (PairingState (BNull, d) b ("Syntax error with early right bracket: " ++ (show bt)))
                    (find (\(PairingState (_,dl) (btr,_) _) ->
                        dl == d && btr == BNull
                    ) acc)
                newLeftB@(nlbt, _) = leftB oldState
                newRightB@(nrbt, _) = b
                newState = 
                    if hasBracketSymmetry nlbt nrbt then
                        PairingState newLeftB newRightB (show nlbt ++ " paired with " ++ show nrbt)
                    else
                        PairingState newLeftB newRightB ("Err: expected " ++ (show $ correctRightBracket nlbt) ++ " got " ++ show nrbt)

            in  (acc \\ [oldState]) ++ [newState]
        
    ) [] -- acc is [PairingState]
    -- on bs, [Bracket]

hasBracketSymmetry :: BracketType -> BracketType -> Bool
hasBracketSymmetry bt1 bt2 =
    case bt1 of
        LPar -> bt2 == RPar
        LSqr -> bt2 == RSqr
        LCur -> bt2 == RCur
        LArr -> bt2 == RArr

correctRightBracket :: BracketType -> BracketType
correctRightBracket bt =
    case bt of
        LPar -> RPar
        LSqr -> RSqr
        LCur -> RCur
        LArr -> RArr