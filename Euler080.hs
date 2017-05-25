import Data.Char
import Math.NumberTheory.Powers.Squares

main = print $ sum $ map sumDigs [1..100]

sumDigs :: Integer -> Int
sumDigs n
  | isSquare n = 0
  | otherwise  = sum $ map digitToInt $ take 100 $ show $ (integerSquareRoot (n * 10^300))
