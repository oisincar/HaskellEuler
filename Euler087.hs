import Data.List
import Data.Set (size, toList, fromList)
import Math.NumberTheory.Primes

main = print $ count $ allP 50000000

allP :: Integer -> [Integer]
allP bound = allPs bound 0 4
  where
    allPs :: Integer -> Integer -> Integer -> [Integer]
    allPs _     total 1 = [total]
    allPs bound total pow = concat [allPs (bound - pp) (total + pp) (pow-1) | pp <- pps]
      where pps = takeWhile (< bound) $ map (^pow) primes

count lst = size $ fromList lst
