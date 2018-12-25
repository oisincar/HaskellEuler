{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import qualified Data.Set as S

negTimes a b = -a * b
negDiv a b = -a / b
ops = [(+), (-), (/), (*), negTimes, negDiv]

-- All sets of 4 unique digits, 1-9
allLsts :: [[Int]]
allLsts = allL' 9 4
  where allL' x l
          | l == 0         = [[]]
          | l < 0 || x < 1 = []
          | otherwise      = allL' (x-1) l ++ (map (x:) (allL' (x-1) (l-1)))

allVal :: [Int] -> [Int]
allVal lst = S.toList $ S.fromList [ round v | f1 <- ops, f2 <- ops, f3 <- ops,
                                         lst' <- permutations lst,
                                         let v = eval [f1, f2, f3] $ map fromIntegral lst',
                                         v > 0,
                                         floor v == ceiling v]

numConsec :: [Int] -> Int
numConsec l = length $ takeWhile (== True) $ zipWith (==) l [1..]

eval :: [(a -> a -> a)] -> [a] -> a
eval [] (x:_) = x
eval (f:fs) (x:y:ys) = eval fs ((f x y) : ys)

-- Pairs of the num consec, and the list that created it.
res = [(numConsec . allVal $ lst, lst) | lst <- allLsts]

best = concatMap show . reverse . snd . maximum $ res
main = putStrLn best
