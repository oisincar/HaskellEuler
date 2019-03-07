import Math.NumberTheory.Primes

bounces = 12017639147
-- bounces = 1000001
-- bounces = 11
sideLen = ((bounces - 1) `div` 2) +2
fs = map fst $ factorise sideLen -- Note: All factors = 2 mod 3, same as stOffset.
stOffset = 3-(sideLen `mod` 3)

-- How many numbers in the range x `elem` [1..sideLen-1] s.t. x shares no factors with sideLen, and x `mod` 3 = 2 (stOffset)
ansSimple = length [(sideLen, x) | x <- [stOffset, stOffset +3 .. sideLen-1], all (\a -> x `mod` a /= 0) fs]

subSets = tail . allSubsets
  where allSubsets [] = [[]]
        allSubsets (a:as) = allSubsets as ++ map (a:) (allSubsets as)

-- Add if odd length, subtract otherwise
inclusion = map f $ subSets fs
  -- where f lst = (sideLen `div` product lst)
  where
    f :: [Integer] -> Integer
    f lst = ((sideLen - (fromIntegral (length lst))) `div` product lst)
          * (if length lst `mod` 2 == 1 then 1 else -1)

sumSeries n = (n*n + n) `div` 2

main = print $ (sideLen - (sum $ map (+1) inclusion)) `div` 3
