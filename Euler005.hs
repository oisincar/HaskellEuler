import           Data.List
import           Data.Maybe

main = print $ tstCase 20

tstCase num = product (zipWith (^) primes (primeMults num))
  where primeMults num = foldr fPrimes (head primeFacts) (take num primeFacts) -- head primefacts is all 0s
        primeFacts = map timesPrimes [1..40]

-- Uhh have fun with this one...
primes = [2,3,5,7,11,13,17,19,23,29,31,37]
timesPrimes = timesPrimes' primes
timesPrimes' (prime:primesA) p = timePrime prime p : timesPrimes' primesA p
timesPrimes' [] _ = []
timePrime prime p
    | p `mod` prime == 0 = 1 + timePrime prime (p `div` prime)
    | otherwise = 0

fPrimes (prime:primes) (prime':primes') = max prime prime' : fPrimes primes primes'
fPrimes _ _ = []
