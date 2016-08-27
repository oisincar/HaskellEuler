
main = print ans
  where ans = length $ filter id $ map (\x -> doesMonthStart $ take x allmonths) [0..length allmonths]

months :: Bool -> [Int]
months b
  | b         = [31,29,31,30,31,30,31,31,30,31,30,31]
  | otherwise = [31,28,31,30,31,30,31,31,30,31,30,31]

isLeap :: Int -> Bool
isLeap year = year `mod` 4 == 0

allmonths = concatMap (months . isLeap) [1901..2000]

doesMonthStart :: [Int] -> Bool
doesMonthStart months = (sum months + 365) `mod` 7 == 6
