import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Map as M

data CaveType = Small | Large | Terminal deriving (Show, Eq, Ord)
type Node = (String, CaveType)
type Graph = [(Node, Node)]

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    print $ length . traverseGraph ("start", Terminal) [] $ parseGraph inputStr

parseGraph :: String -> Graph
parseGraph =
    foldStringMap .
    map (\s -> splitOn "-" $ s) .
    splitOn "\n"

foldStringMap :: [[String]] -> Graph
foldStringMap = 
    snd .
    foldl (\(accM, accG) [nStr1, nStr2] -> -- [n1, n2] == [String, String]
        let typeFor nStr = 
                if nStr `elem` (M.keys accM) then
                    accM M.! nStr
                else 
                    caveTypeForAlpha $ head nStr
            nCT1 = typeFor nStr1
            nCT2 = typeFor nStr2
            newAccM =
                M.insert nStr1 nCT1 .
                M.insert nStr2 nCT2 $
                accM
            newAccG = ((nStr1, nCT1), (nStr2, nCT2)):accG
        in  (newAccM, newAccG)
    ) 
    (M.fromList 
        [("start", Terminal), ("end", Terminal)]
    , [])
    -- on [[String]] list of tuples-as-list. 
    -- acc is (Map String CaveType, [(Node, Node)]) the map between strings and their node representation, alongside the accumulated represented node pairings

caveTypeForAlpha :: Char -> CaveType
caveTypeForAlpha c = 
    let asciiC = ord c 
    in  if asciiC >= 97 then Small else Large

traverseGraph :: Node -> [Node] -> Graph -> [[Node]]
traverseGraph curNode@(_, ct) smallVisited g = 
    let newSmallVisited = if ct == Small then curNode:smallVisited else smallVisited
        options = delete ("start", Terminal) $ (adjacentNodes curNode g) \\ newSmallVisited
    in  if curNode == ("end", Terminal) then 
            [[curNode]]
        else 
            map (curNode:) (concatMap (\n -> traverseGraph n newSmallVisited g) options)


adjacentNodes :: Node -> Graph -> [Node]
adjacentNodes n = 
    foldl (\acc (n1, n2) ->
        if n == n1 then
            n2 : acc 
        else if n == n2 then
            n1 : acc
        else
            acc
    ) []
