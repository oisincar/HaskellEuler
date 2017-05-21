import Data.MemoCombinators
import Data.List

main = print ans

ans = (\(_,n,_) -> n) $ minimum $ filter (\(_,n,phi) -> strr n == strr phi && n /= 1) $ search primes 1 1
  where strr = sort . show

bound = 10^7

-- go through literally all numbers up to 10^7, computing phi as we go.
search :: [Int] -> Double -> Int -> [(Double, Int, Int)]
search (p:ps) phi sum
  | sum*p >= bound = [(1/phi, sum, round ((fromIntegral sum)*phi))]
  | otherwise =
    (concatMap (search ps (phi * (1-1/(fromIntegral p)))) $ takeWhile (< bound) [sum * p^n | n <- [1..]])
    ++ (search ps phi sum)

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
