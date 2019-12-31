import Math.NumberTheory.Primes
import Debug.Trace

bound = 100000000

-- All primes that could make up a composite num
allPs = takeWhile (<= (bound `div` 2)) primes
-- Store indices
bounds = reverse allPs

-- Primes, Bound, amount advanced through primes, total ps
calc :: [Integer] -> [Integer] -> Integer -> Integer
calc (p:ps) (b:bs) dst -- | trace ("calc " ++ show p ++ " " ++ show b ++ " " ++ show dst) False = undefined
  | p > b = 0
  | p * b <= bound = (calc ps (b:bs) (dst-1)) + dst
  | otherwise      = (calc (p:ps) bs (dst-1))

main = do
  print $ calc allPs bounds numPs
    where numPs = fromIntegral $ length allPs
