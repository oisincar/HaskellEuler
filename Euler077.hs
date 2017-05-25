import Data.List
import Data.Maybe

main = print $ fromJust $ find (\x -> waysToAdd x primes > 5000) [1..]

waysToAdd x (p:ps)
  | x == 0 = 1
  | x < p = 0
  | otherwise = waysToAdd (x-p) (p:ps)
              + waysToAdd (x) (ps)

primes = takeWhile (<1000000) $ 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
