import Data.List.Split
import Data.List
import qualified Data.Map as M

main :: IO ()
main = do
    --inputStr <- readFile "input_test.txt"
    print $ parseEnergyGrid $ inputStr
    --mapM_ print $ take 3 . iterate stepF . parseEnergyGrid $ inputStr

inputStr :: String
inputStr = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

-- Debug --

coordMap :: M.Map (Int, Int) Int
coordMap = M.fromList [((0,0),5),((0,1),4),((0,2),8),((0,3),3),((0,4),1),((0,5),4),((0,6),3),((0,7),2),((0,8),2),((0,9),3),((1,0),2),((1,1),7),((1,2),4),((1,3),5),((1,4),8),((1,5),5),((1,6),4),((1,7),7),((1,8),1),((1,9),1),((2,0),5),((2,1),2),((2,2),6),((2,3),4),((2,4),5),((2,5),5),((2,6),6),((2,7),1),((2,8),7),((2,9),3),((3,0),6),((3,1),1),((3,2),4),((3,3),1),((3,4),3),((3,5),3),((3,6),6),((3,7),1),((3,8),4),((3,9),6),((4,0),6),((4,1),3),((4,2),5),((4,3),7),((4,4),3),((4,5),8),((4,6),5),((4,7),4),((4,8),7),((4,9),8),((5,0),4),((5,1),1),((5,2),6),((5,3),7),((5,4),5),((5,5),2),((5,6),4),((5,7),6),((5,8),4),((5,9),5),((6,0),2),((6,1),1),((6,2),7),((6,3),6),((6,4),8),((6,5),4),((6,6),1),((6,7),7),((6,8),2),((6,9),1),((7,0),6),((7,1),8),((7,2),8),((7,3),2),((7,4),8),((7,5),8),((7,6),1),((7,7),1),((7,8),3),((7,9),4),((8,0),4),((8,1),8),((8,2),4),((8,3),6),((8,4),8),((8,5),4),((8,6),8),((8,7),5),((8,8),5),((8,9),4),((9,0),5),((9,1),2),((9,2),8),((9,3),3),((9,4),7),((9,5),5),((9,6),1),((9,7),5),((9,8),2),((9,9),6)]

parseEnergyGrid :: String -> M.Map (Int, Int) Int
parseEnergyGrid str = 
    M.fromList .
    concat .
    map (\(i,xs) ->
        map (\(j, x) -> 
            ((i, j), x)
        ) $ zip [0..] xs
    ) $ zip [0..] (map (map (\c -> readInt [c])) . splitOn "\n" $ str)
        where readInt s = read s :: Int

stepF :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
stepF = 
    addFlashes [] .
    M.map(\v -> v + 1)

addFlashes :: [(Int, Int)] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
addFlashes pastFlashedCoords eMap =
    let flashingCoords = (M.keys . M.filter (\v -> v == 0 || v > 9) $ eMap) \\ pastFlashedCoords
        nextMap = 
            foldl (\accM crd ->
                foldl (\accMA crdA -> 
                    M.adjust (\v -> if v > 9 then 0 else v + 1) crdA accMA
                ) accM (adjacentCoords crd)
            ) eMap flashingCoords
        newFlashedCoords = pastFlashedCoords ++ (M.keys . M.filter (\v -> v == 0 || v > 9) $ nextMap)
    in  if 0 `elem` (M.elems nextMap) || any (>9) (M.elems nextMap) then
            addFlashes newFlashedCoords nextMap
        else
            nextMap

adjacentCoords :: (Int, Int) -> [(Int, Int)]
adjacentCoords (i, j) = delete (i, j) [(i + i', j + j') | i' <- [(-1)..1], j' <- [(-1)..1]]