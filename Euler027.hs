
import           Data.List

main = print $ snd $ maximum numCons
  where numCons = filter (\(a,_) -> a > 10) $ map numConsecutive abPairs

abPairs :: [(Integer, Integer)]
abPairs = concatMap (\x -> zip (repeat x) [2..999]) [-999..999]

numConsecutive (a,b) = (numCon (a,b) 0, a*b)
    where numCon (a,b) n
            | n > 1000 = 9999
            | isPrime (n*n + a*n + b) = 1 + (numCon (a,b) (n+1))
            | otherwise = 0

isPrime :: Integer -> Bool
isPrime p
  | p <= 1 = False
  | otherwise = null [x | x <- 2:[3,5..isqrt p], p `mod` x == 0]

isqrt = ceiling . sqrt . fromIntegral
