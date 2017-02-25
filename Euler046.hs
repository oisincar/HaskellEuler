import Data.Set (Set)
import qualified Data.Set as Set

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

compNums = search primes [3,5..]
  where search (p:ps) (c:cs)
          | c == p = search ps cs
          | c > p  = search ps (c:cs)
          | otherwise = c : search (p:ps) (cs)

isValid pSet n = any (\s -> Set.member (n - 2*s*s) pSet) [1.. intSqrt n]
  where intSqrt = floor . sqrt . fromIntegral

main = print ans
  where ans = head $ filter (not . isValid pSet) compNums
        pSet = Set.fromList $ take 1000 primes
