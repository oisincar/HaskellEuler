{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Ord
import Data.Function
import Math.NumberTheory.Primes
 -- Trying to find a product-sum number for some large k

upB = 18000 -- upper bound for all nums
kMin = 2
kMax = 12000

-- Number, prime factors for all numbers up to upB
allPrimeFacts :: [(Integer, [Integer])]
allPrimeFacts = sort $ primeFacts primes 1 []
  where
    primeFacts (p:ps) tot facts
      | tot*p > upB = [(tot, facts)]
      | otherwise   = primeFacts ps tot facts ++ primeFacts (p:ps) (tot*p) (p : facts)

-- Group by length. [[(Num occurences, number)]]
allPrimeFactsG :: [[(Int, Integer)]]
allPrimeFactsG = map describe allPrimeFacts
  where describe ps = map (\a -> (length a, head a)) $ group . snd $ ps

-- Stupid method, pretty ineffecient but works.
allFactorizations :: [(Int, Integer)] -> [[Integer]]
allFactorizations facts = allFact' $ concatMap (\(c, n) -> replicate c n) facts
  where
    allFact' :: [Integer] -> [[Integer]]
    allFact' [] = [[]]
    allFact' (p:ps) = nub $ (map (p:) rec) ++ (concatMap (\r -> map (sort . mulOneElem p r) [0..length r - 1]) rec)
      where rec = allFact' ps

-- Multiply a single element by a given number. I should learn lens...
mulOneElem :: Integer -> [Integer] -> Int -> [Integer]
mulOneElem p lst ix = let (st, n:end) = splitAt ix lst in st ++ (p*n : end)

-- For a number and all it's factors, list all the values of k it can satisfy
allKs :: [[Integer]]
allKs = map ks $ zip [1..] (map allFactorizations allPrimeFactsG)
  where
    ks :: (Integer, [[Integer]]) -> [Integer]
    ks (n, ffs) = map (\fs -> n - sum fs + (fromIntegral . length $ fs)) ffs

-- [(k, sum/product)]
allKsTuple :: [(Integer, Integer)]
allKsTuple = concat $ zipWith a allKs [1..]
  where a = (\ks n -> (map (,n) ks))

main = print $ sum . nub $ bestKs
  where
    bestProductSums = map head $ groupBy ((==) `on` fst) $ sort allKsTuple
    -- 2 <= k <= 12000 ...
    bestKs = map snd . take (12000-2) . tail $ bestProductSums
