import Data.Array
import Data.Char

main = print ans

ans = length $ filter (== 60) $ map depthNode [1..10^6]

depthNode n
  | n < 10^6  = arr ! n
  | otherwise = depth n
    where arr = array (0, 10^6) [(n, depth n) | n <- [1..10^6]]
          depth n
            | n `elem` [871, 45361, 872, 45362] = 2
            | n `elem` [169, 363601, 1454] = 3
            | step n == n = 1
            | otherwise = 1 + depthNode (step n)

step n = sum $ map (fact . digitToInt) $ show n
  where fact n = (scanl (*) 1 [1..]) !! n
