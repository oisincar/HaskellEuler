import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

maxSum :: [Integer] -> Set Integer -> Integer -> Integer -> (Integer, Integer)
maxSum (p:ps) pSet sum count
  | sum > 1000000 = (-1,0)
  | Set.member sum pSet = max (count, sum) recc
  | otherwise = recc
  where recc = maxSum ps pSet (sum + p) (count + 1)
maxSum _ _ _ _ = (-1,0)

ansRec (p:ps) pSet = max (maxSum (p:ps) pSet 0 0) $ ansRec ps pSet
ansRec _ _ = (0,0)

ans = ansRec p pS
  where p = takeWhile (< 1000000) primes
        pS = Set.fromList p

main = print $ snd ans
