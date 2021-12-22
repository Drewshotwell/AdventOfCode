import Data.List
import Data.Char

main :: IO ()
main = do
    inputStr <- readFile "./input.txt"
    print $ let binReadings = words inputStr in (o2Rating binReadings) * (co2Rating binReadings)

bitShare :: Int -> String -> Int
bitShare b = length . filter (==(intToChar b))

o2Rating :: [String] -> Int
o2Rating binNums = binToDec (read . head $ processBins binNums 0 (>=)) :: Int

co2Rating :: [String] -> Int
co2Rating binNums = binToDec (read . head $ processBins binNums 0 (<)) :: Int

processBins :: [String] -> Int -> (Int -> Int -> Bool) -> [String]
processBins [bStr]  _ _     = [bStr]
processBins binNums i compF 
    | i >= (length . head $ binNums) = error "Couldn't determine a rate: index too large"
    | otherwise =
        let bitsAtI = map (!!i) binNums
            mostCommonBit = if compF (bitShare 1 bitsAtI) (bitShare 0 bitsAtI) then '1' else '0'
            resultingBins = filter (\b -> b !! i == mostCommonBit) binNums
        in  processBins resultingBins (i + 1) compF

binToDec :: Int -> Int
binToDec b =
    let bDigs = numToDigits b
    in  foldl (\acc (d,e) -> acc + d*(2^e)) 0 $ zip bDigs (reverse [0..length bDigs - 1])

numToDigits :: Int -> [Int]
numToDigits = map digitToInt . show

intToChar :: Int -> Char
intToChar x = chr (x + 48)