
import           Data.Char

main = print $ sum [x | x <- [2..354294], check x]

check :: Int -> Bool
check num = num == digSum
  where digSum = sum $ map (\c -> digitToInt c^5) (show num)
