import Data.List
import Data.Char

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    print $ let binReadings = words inputStr in (gammaRate binReadings) * (epsilonRate binReadings)

bitShare :: Int -> String -> Int
bitShare b = length . filter (==(intToChar b))

relationalRate :: [String] -> (Int -> Int -> Bool) -> Int
relationalRate binNums compF =
    binToDec . digitsToNum
        $ map (\bStr -> if compF (bitShare 1 bStr) (bitShare 0 bStr) then 1 else 0)
        $ map (\i -> map (!!i) binNums) [0..length (head binNums) - 1]

gammaRate :: [String] -> Int
gammaRate binNums = relationalRate binNums (>)

epsilonRate :: [String] -> Int
epsilonRate binNums = relationalRate binNums (<)

numToDigits :: Int -> [Int]
numToDigits = map digitToInt . show

digitsToNum :: [Int] -> Int
digitsToNum numL = (read . concatMap show $ numL) :: Int

binToDec :: Int -> Int
binToDec b =
    let bDigs = numToDigits b
    in  foldl (\acc (d,e) -> acc + d*(2^e)) 0 $ zip bDigs (reverse [0..length bDigs - 1])

intToChar :: Int -> Char
intToChar x = chr (x + 48)