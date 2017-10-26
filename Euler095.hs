import Data.Graph
import Data.Ord
import Data.List
import Data.Tree
-- import Math.NumberTheory.Primes.Factorisation
import Math.NumberTheory.ArithmeticFunctions

main = do
  print ans
    where
      ans = minimum longestScc
      longestScc = maximumBy (comparing length) sccs
      sccs = (map flatten) $ scc $ graph 999999

sumDivisors :: Int -> Int
sumDivisors n = (fromIntegral . sum . divisors . toInteger $n) - n

graph s = buildG (0, s+1) [(n,  trunc n) | n <- [1..s]]
  -- Connect all edges going out of bounds to s+1. Won't be considered for scc
  where trunc n = min s (sumDivisors n)


-- WORKING: Do divisors from prime factor subsets.
-- sumDivisors :: Int -> Int
-- sumDivisors n = fromIntegral (sum . divis . factorise . toInteger $ n) - n

-- divis :: [(Integer, Int)] -> [Integer]
-- divis [] = [1]
-- divis ((p, i):ps) = (concat $ rec : [ map (p^ii *) rec | ii <- [1.. toInteger i]])
--   where rec = divis ps
