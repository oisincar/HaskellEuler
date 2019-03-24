import qualified Data.MemoCombinators as Memo

main = print $ combos False 50

combos :: Bool -> Integer -> Integer
combos = Memo.memo2 Memo.bool Memo.integral combos'
  where 
    combos' last rem
      | rem < 0  = 0
      | rem == 0 = 1
      | last == False = combos True (rem - 3) + combos False (rem - 1)
      | otherwise     = combos True (rem - 1) + combos False (rem - 1)
