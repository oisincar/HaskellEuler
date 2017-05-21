import Data.MemoCombinators
import Data.List

-- Answer using more logic; answers with low phi have many unique factors.
-- Number with most unique will be the largest number made by multiplying together the primes.
main = print $ last $ takeWhile (< 1000000) $ scanl (*) 1 primes

-- Answer using brute force search. Takes ~1m.
-- main = print maximum [((fromIntegral n)/(fromIntegral (phi n)), n) | n <- [1..1000000]]

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

factors = factors' primes
  where factors' (p:ps) n
          | n == 1         = []
          | n `mod` p == 0 = p: factors' (p:ps) (n `div` p)
          | otherwise      = factors' ps n

phi n
  | length facts == 1 = n -1
  | otherwise         = n * (product (map (\x -> x-1) uniqueFacts)) `div` (product uniqueFacts)
  where
    facts = factors n
    uniqueFacts = map head $ group $ facts
