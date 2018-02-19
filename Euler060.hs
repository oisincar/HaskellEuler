import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Testing
import qualified Data.Set as S

main = print ans

isConcatPrime :: Integer -> Integer -> Bool
isConcatPrime a b = (isPrime $ read $ show a ++ show b)
                 && (isPrime $ read $ show b ++ show a)

  --              PrimeSets, SmallerPrimes
g :: Integer -> ([S.Set Integer], S.Set Integer) -> ([S.Set Integer], S.Set Integer)
g p (primeSets, smPrimes) = (newPrimeSets, S.insert p smPrimes)
  where
    -- Numbers this prime is concatable with.
    primePairs = S.filter (isConcatPrime p) smPrimes
    -- Groups this prime is concatable with the entirity of.
    nSets = filter (\x -> S.isSubsetOf x primePairs) primeSets
    -- New complete groups.
    newPrimeSets = primeSets               -- old sets
                ++ map (S.insert p) nSets  -- new sets w/ added p
                ++ [ S.singleton p ]       -- current prime

-- List of all prime sets.
f = fst $ foldl (flip g) ([], S.empty) (take 2000 primes)
-- First one with 5 in it is the answer.
ans = S.foldr (+) 0 $ head $ filter (\x -> S.size x >= 5) f
