{-# LANGUAGE RankNTypes #-}

import qualified Data.MemoCombinators as Memo
import Numeric
import Data.Char

main = putStrLn . map toUpper $ showHex (solve 16) ""

solve :: Int -> Integer
solve 0 = 0
solve n = (solve (n-1)) -- put '0'
        + 13 * sInNum (n-1) False False False -- 13 regular chars
        + sInNum (n-1) False True False -- Add '1'
        + sInNum (n-1) False False True -- Add 'A'

sInNum :: Int -> Bool -> Bool -> Bool -> Integer
sInNum = memo4 Memo.integral Memo.bool Memo.bool Memo.bool solve'
-- sInNum = solve'
  where
    solve' 0 True True True = 1
    solve' 0 _ _ _ = 0
    solve' n a b c = (13 * sInNum (n-1) a b c)
                   + (sInNum (n-1) True b c)
                   + (sInNum (n-1) a True c)
                   + (sInNum (n-1) a b True)

-- Memo a 4 argument function!!
memo4 :: Memo.Memo a -> Memo.Memo b -> Memo.Memo c -> Memo.Memo d -> (a -> b -> c -> d -> r) -> (a -> b -> c -> d -> r)
memo4 a b c d = a . (Memo.memo3 b c d .)
