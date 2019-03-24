import qualified Data.MemoCombinators as Memo

main = print $ combos 50

combos :: Integer -> Integer
combos = Memo.integral combos'
  where
    combos' :: Integer -> Integer
    combos' rem
          | rem < 0   = 0 -- neg = wrong
          | rem <= 1  = 1 -- 0 or 1 left, only one possible ans
          | otherwise = sum [combos (rem - jump) | jump <- [1..4]]
