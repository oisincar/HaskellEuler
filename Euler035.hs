import Data.Array
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

primesTo m = sieve [2..m]
             where
             sieve (x:xs) = x : sieve (xs Data.List.\\ [x,x+x..m])
             sieve [] = []

-- primes = fromList $ primesTo 1000000

-- primes2 :: [Integer]
-- primes2 = 2 : 3 : ([5,7..] (-) _U [[p*p, p*p+2*p..] | p <- tail primes2])



primes = takeWhile (<1000000) $ 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

primesSet = Set.fromList $ primes

circP = filter isCircular primes


isCircular :: Int -> Bool
isCircular n = circ (length (show n)) (show n)
  where circ 0 _ = True
        circ n (x:xs) = (Set.member ((read::String->Int) rotStr) primesSet) && (circ (n-1) rotStr)
          where rotStr = xs ++ [x]

main = print $ length circP
