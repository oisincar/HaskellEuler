import Data.Array
import Data.Char

fact = listArray (0,9) (1 : map (\x -> (fact ! (x-1)) * x) [1..9])

isValid n = n == (sum $ map (\x -> fact!x) lstInts)
  where lstInts = map digitToInt (show n)

ans = sum $ filter isValid [3..10000000]

main = print ans
