import Data.List

-- SHIT'S HARD YO

primeFFact :: Integer -> [Integer]
primeFFact n = sort $ concatMap primeFactors [2..n]

primeFactors :: Integer -> [Integer]
primeFactors = pF primes
  where pF (p:ps) n
          | n == 1         = []
          | n `mod` p == 0 = p : pF (p:ps) (n `div` p)
          | otherwise      = pF ps n

primes = takeWhile (<1000000) $ 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
