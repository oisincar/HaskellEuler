import Data.List

main = do
  numsStr <- lines <$> readFile "./p089_roman.txt"
  print $ sum $ map saving numsStr

saving :: String -> Int
saving str = (length str) - (length . writeRoman . readRoman $ str)

readRoman :: String -> Int
readRoman = readRoman' . group

readRoman' :: [String] -> Int
readRoman' (d:ds)
 | ds == []               =  dVal
 | dVal > readG (head ds) =  dVal   + readRoman' ds
 | otherwise              = (-dVal) + readRoman' ds
 where dVal = readG d
       readG ds = length ds * (readDigit . head $ ds)

readDigit 'I' = 1
readDigit 'V' = 5
readDigit 'X' = 10
readDigit 'L' = 50
readDigit 'C' = 100
readDigit 'D' = 500
readDigit 'M' = 1000

writeRoman :: Int -> String
writeRoman val
  | val < 10  = writeLT10 val
  | val > 1000 = 'M' : writeRoman (val-1000)
  | otherwise = concatMap times10 (writeRoman $ val `div` 10) ++ writeLT10 (val `mod` 10)

writeLT10 :: Int -> String
writeLT10 i
  | i == 4 = "IV"
  | i == 9 = "IX"
  | i >= 5 = 'V' : writeLT10 (i - 5)
  | otherwise = replicate i 'I'

times10 'I' = "X"
times10 'V' = "L"
times10 'X' = "C"
times10 'L' = "D"
times10 'C' = "M"
times10 'D' = "MMMMM"
times10 'M' = "MMMMMMMMMM"
