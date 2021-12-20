import Data.List
import Data.List.Split

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    let readings = [parseReading reading | reading <- splitOn "\n" inputStr]
    print $ intersections . map lineCoordinates $ readings

parseReading :: String -> ((Int, Int), (Int, Int))
parseReading =
    (\tuple -> 
        let createTupleElem s = let sSplit = splitOn "," s in (readInt $ sSplit !! 0, readInt $ sSplit !! 1)
        in  (createTupleElem (fst tuple), createTupleElem (snd tuple))) .
    \s -> let sSplit = splitOn "->" s in (sSplit !! 0, sSplit !! 1)

lineCoordinates :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
lineCoordinates ((p1x, p1y), (p2x, p2y)) =
    let delX = p2x - p1x
        delY = p2y - p1y
        gcdScaler = gcd delX delY
        gcdVec = (delX `div` gcdScaler, delY `div` gcdScaler)
    in  [(p1x, p1y) `addTuple` (scaleTuple gcdVec k) | k <- [0..gcdScaler]]

intersections :: [[(Int, Int)]] -> Int
intersections = length . filter (\xs -> length xs >= 2) . group . sort . concat

readInt :: String -> Int
readInt s = read s :: Int

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (p1x, p1y) (p2x, p2y) = (p1x + p2x, p1y + p2y)

scaleTuple :: (Int, Int) -> Int -> (Int, Int)
scaleTuple (px, py) scaler = (px*scaler, py*scaler)