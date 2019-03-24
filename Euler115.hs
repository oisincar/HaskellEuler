import qualified Data.MemoCombinators as Memo

main = print . length . takeWhile (< 1000000) $ map (combos False 50) [0..]

combos :: Bool -> Integer -> Integer -> Integer
combos = Memo.memo3 Memo.bool Memo.integral Memo.integral combos'
  where 
    combos' last minS rem
      | rem < 0  = 0
      | rem == 0 = 1
      | last == False = combos True minS (rem - minS) + combos False minS (rem - 1)
      | otherwise     = combos True minS (rem - 1)    + combos False minS (rem - 1)
