import           Data.List
import           Data.Maybe

main = print $ ans 2000000
  where ans n = sum $ takeWhile (< n) primes

primes :: [Integer]
primes = 2: 3: sieve 0 (tail primes) 3
sieve k (p:ps) x = [n | n <- [x+2,x+4..p*p-2], and [n`rem`p/=0 | p <- fs]] ++ sieve (k+1) ps (p*p)
  where fs = take k (tail primes)
