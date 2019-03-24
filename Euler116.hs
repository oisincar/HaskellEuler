import qualified Data.MemoCombinators as Memo

main = print ans

ans = combos 2 len + combos 3 len + combos 4 len
  where len = 50

-- Exclude 0 sol
combos :: Integer -> Integer -> Integer
combos tLen rem = (c tLen rem) -1
  where
    c = (Memo.memo2 Memo.integral Memo.integral combos')
    combos' :: Integer -> Integer -> Integer
    combos' tLen rem
          | rem < tLen = 1
          | otherwise  = (c tLen (rem - 1)) + (c tLen (rem - tLen))
