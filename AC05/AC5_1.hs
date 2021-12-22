import Data.List
import Data.List.Split

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    let readings = [parseReading reading | reading <- splitOn "\n" inputStr]
    let straightReadings = straightLines readings
    print $ intersections . map lineCoordinates $ straightReadings

parseReading :: String -> ((Int, Int), (Int, Int))
parseReading =
    (\tuple -> 
        let createTupleElem s = let sSplit = splitOn "," s in (readInt $ sSplit !! 0, readInt $ sSplit !! 1)
        in  (createTupleElem (fst tuple), createTupleElem (snd tuple))) .
    \s -> let sSplit = splitOn "->" s in (sSplit !! 0, sSplit !! 1)

straightLines :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
straightLines = filter (\((p1x, p1y), (p2x, p2y)) -> (p1x == p2x) || (p1y == p2y))

lineCoordinates :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
lineCoordinates ((p1x, p1y), (p2x, p2y)) =
    let delX = p2x - p1x
        delY = p2y - p1y
        deltaVecStep = if abs delX > abs delY then (1, 0) else (0, 1)
        deltaVecMax = if abs delX > abs delY then delX else delY
        scalerRanger = if deltaVecMax > 0 then [0..deltaVecMax] else [0,(-1)..deltaVecMax]
    in  [(p1x, p1y) `addTuple` (scaleTuple deltaVecStep k) | k <- scalerRanger]

intersections :: [[(Int, Int)]] -> Int
intersections = length . filter (\xs -> length xs >= 2) . group . sort . concat

readInt :: String -> Int
readInt s = read s :: Int

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (p1x, p1y) (p2x, p2y) = (p1x + p2x, p1y + p2y)

scaleTuple :: (Int, Int) -> Int -> (Int, Int)
scaleTuple (px, py) scaler = (px*scaler, py*scaler)