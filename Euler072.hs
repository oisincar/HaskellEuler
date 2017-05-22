import Data.List

allNumsPrimes :: [(Int, [Int])]
allNumsPrimes = search primes 1 []
  where search (p:ps) n facts
          | n*p > 10^6 = []
          | otherwise =
            (concatMap (\a -> (a, nfacts) : search ps a nfacts)
            $ takeWhile (<=10^6) (map (\x -> n*p^x) [1..]))
            ++ search ps n facts
          where nfacts = p:facts

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

fractsDivCount (n, fcts) = (n-1) - (sum $ map (\x -> (n-1) `divP` x) $ divLst [] fcts)
  where divLst ls (f:fs) = divLst (f : ls ++ (map (*(-f)) ls)) fs
        divLst ls [] = ls

main = print $ sum $ map fractsDivCount allNumsPrimes

-- div which rounds towards 0
divP :: Int -> Int -> Int
divP a b
  | b >= 0    =    a `div` b
  | otherwise = - (a `div` (-b))
