
import           Data.Char

main = print $ sum $ map digitToInt $ show $ factorial 100

factorial 0 = 1
factorial n = n * factorial (n-1)
