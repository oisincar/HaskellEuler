import Data.Set (Set)
import qualified Data.Set as Set

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

distFactors n (p:ps)
  | n == 1 = 0
  | n `mod` p == 0 = 1 + distFactors (divTil n p) ps
  | otherwise = distFactors n ps
  where divTil n p
          | n `mod` p == 0 = divTil (n `div` p) p
          | otherwise = n

facts = filter (\n -> (distFactors n primes) == 4) [1..]

searchCons c (n1:n2:ns)
  | c == 4 = n1 - 3
  | n1+1 == n2 = searchCons (c+1) (n2:ns)
  | otherwise  = searchCons 1 (n2:ns)

main = print ans
  where ans = searchCons 1 facts
