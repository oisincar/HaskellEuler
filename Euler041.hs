import Data.List

panDig n = map (read::String->Int) $ permutations $ concatMap show [1..n]

isPrime :: Int -> Bool
isPrime n = not $ any (\x -> n `mod` x == 0) $ 2:[3,5.. intSqrt n]
  where intSqrt = floor . sqrt . fromIntegral

ans = last $ sort $ filter isPrime (concatMap panDig [1..9])
