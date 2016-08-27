import           Data.List
import           Data.Maybe

main = print $ ans 500

ans :: Int -> Integer
ans num = snd $ fromJust $ find (\(x,y) -> x > num) triangleFacPairs

triangleFacPairs = map (\x -> (numFactors x, x)) triangleNums

triangleNums = tNums 1 0
    where tNums n s = sum : tNums (n+1) sum
            where sum = n+s

numFactors :: Integer -> Int
numFactors 1 = 1
numFactors num
    | isSquare  = 2*halfFac -1
    | otherwise = 2*halfFac
    where squrt = floor $ sqrt $ fromIntegral num
          isSquare = squrt^2 == num
          halfFac = length $ filter (\x -> num `mod` x == 0) [1..squrt]
