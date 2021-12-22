import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe
import Data.Time
import qualified Data.Map as M

data CaveType = Small | Large | Terminal deriving (Show, Eq, Ord)
type Node = (String, CaveType)
type Graph = [(Node, Node)]

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    let readGraph = parseGraph inputStr
    start <- getCurrentTime
    print $ length $ traverseWithSv2 readGraph
    stop <- getCurrentTime
    print $ "Finished in: " ++ (show $ diffUTCTime stop start)

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
    -- acc is (Map String CaveType, [(Node, Node)]) the map between strings and their node representation,
    -- alongside the accumulated represented node pairings

caveTypeForAlpha :: Char -> CaveType
caveTypeForAlpha c = 
    let asciiC = ord c 
    in  if asciiC >= 97 then Small else Large

traverseGraph :: Node -> [Node] -> Maybe Node -> Graph -> [[Node]]
traverseGraph curNode@(_, ct) smallVisited sv2 g = 
    -- sv2 is the small node that was visited twice. '
    -- Since its protection can be exhausted, we wrap it with a temporary Maybe
    let sv2Hit = (not . isNothing $ sv2) && (fromJust sv2 == curNode)
        newSmallVisited = if ct /= Small || sv2Hit then smallVisited else curNode:smallVisited 
        newSv2 = if sv2Hit then Nothing else sv2
        options = delete ("start", Terminal) $ (adjacentNodes curNode g) \\ newSmallVisited
    in  if curNode == ("end", Terminal) then 
            [[curNode]]
        else 
            map (curNode:) (concatMap (\n -> traverseGraph n newSmallVisited newSv2 g) options)

traverseWithSv2 :: Graph -> [[Node]]
traverseWithSv2 g =
    let smallNodes = 
            nub . 
            foldl (\acc (n1@(_, ct1), n2@(_, ct2)) ->
                if ct1 == Small then
                    n1 : acc
                else if ct2 == Small then
                    n2 : acc
                else
                    acc
            ) [] $ g
        smallNodesMaybe = map (\n -> Just n) smallNodes
    in  nub . concatMap (\sn -> traverseGraph ("start", Terminal) [] sn g) $ smallNodesMaybe

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
