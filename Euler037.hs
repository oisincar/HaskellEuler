import Data.Set (Set)
import qualified Data.Set as Set

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

ansLst2 :: Set Int -> [Int] -> Int -> [Int]
ansLst2 pSet (p:ps) count
  | count == 0 = []
  | p < 10 = recc count
  | isTrunc truncLeft p &&
    isTrunc truncRight p = p : recc (count - 1)
  | otherwise = recc count

  where isTrunc f p | f p == 0 = True
                    | otherwise = Set.member (f p) pSet && isTrunc f (f p)

        truncLeft p = (p `div` 10)
        truncRight p | p < 10 = 0
                     | otherwise = (read . (drop 1) . show) p
        recc = ansLst2 (Set.insert p pSet) ps

main = print $ sum $ ansLst2 (Set.empty) primes 11
