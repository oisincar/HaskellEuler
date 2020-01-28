import Data.List
import Math.NumberTheory.Primes
import Math.NumberTheory.Powers.Cubes


maxV = 10^18
--maxV = 10000

primesltm = takeWhile (<= (integerCubeRoot maxV)) primes

-- Takes all primes less than bound cube root, and returns all cubeful numbers within bounds.
-- It's kinda slow cause concatination of lists is slow.
cubeful :: [Integer] -> [Integer]
cubeful [] = [1]
cubeful (p:ps) = [ x*p^n
                 | n <- takeWhile (\n ->   p^n <= maxV) [3..]
                 , x <- takeWhile (\x -> x*p^n <= maxV) re
                 ] ++ re
  where re = sort $ cubeful ps

bigS = sum [maxV `div` x | x <- cubeful primesltm]

main = print bigS
