import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

ans = [ show p1 ++ show p2 ++ show p3
      | p2 <- fourDigPs, p1 <- takeWhile (< p2) fourDigPs, p3 <- [p2*2-p1]
      , Set.member p3 pSet
      , (sort . show) p1 == (sort.show) p2
      , (sort . show) p1 == (sort.show) p3
      ]
  where
    pSet = Set.fromList fourDigPs
    fourDigPs = takeWhile (\n -> (length . show) n == 4) $
                dropWhile (\n -> (length . show) n < 4) primes

main = putStrLn $ ans !! 1
