
-- Calculated mod this...
md :: Integer
md = 1000000007

-- Sum from 1 to n
sum1ton n = (n*n + n) `div` 2

-- Sprinkle mods everywhere so I don't have to think.
modTimes :: Integer -> Integer -> Integer
modTimes a b = (a * b) `mod` md
-- a^b  (mod) md
modPow :: Integer -> Integer -> Integer
modPow a b
  | b == 0         = 1
  | b == 1         = a
  | b `mod` 2 == 1 = a `modTimes` (modPow a (b-1))
  | otherwise      = x `modTimes` x
    where x = (modPow a (b `div` 2))

-- Sum of first digit of all the numbers in the bigS sum up to n
sumFirstDigit :: Integer -> Integer
-- (sum 1-9 = 45, )
sumFirstDigit n = (sum1ton 9) * (1-(modPow 10 c)) `div` (1-10) -- Geometric sequence of sums of 1-9 * 10^p
                  + (sum1ton r) * (modPow 10 c)              -- Remaining 1-n (n<9) bit.
  where c = n `div` 9
        r = n `mod` 9

-- 0*9 + 9*9 + 99*9 + 999*9 ... until we have n terms
-- = 9 * (0 + 9 + 99 + 999 + ...)
-- = 9 * (1 + 10 + 100 + 1000 + ...) - n
-- Then adding on remaining bit that doesn't fit into a whole term of
-- the geometric sequence.

-- 9 * ((1 - (modPow 10 c)) `div` (1-10)) -- cancels down to first term here.
sumOtherDigits :: Integer -> Integer
sumOtherDigits n = ((modPow 10 c)-1) -- 9 * (1 + 10 + 100...)
                   + (modPow 10 c) * r  -- Remaining terms not in geometric sequence
                   - n                  -- Convert to 0 + 9 + 99 +..
    where c = n `div` 9
          r = n `mod` 9

bigS :: Integer -> Integer
bigS n = (sumFirstDigit n) + (sumOtherDigits n) `mod` md


fib :: [Integer]
fib = 0:1: (zipWith (+) fib (tail fib))
fibz = take 89 $ (drop 2) fib

main = print $ (sum $ map bigS fibz) `mod` md
