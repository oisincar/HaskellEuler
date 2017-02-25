import Data.Char

ans = foldr (*) 1 $ map (\n -> digitToInt (fract !! n)) powers
  where fract = concatMap show [1..]
        powers = map (\p -> floor (10**p) - 1) [0..6] -- 6

main = print ans
