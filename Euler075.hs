import Data.List

main = print $ length $ filter (\ls -> length ls == 1) $ group $ sort triples

perim = 1500000
triples = [k*2*(m*m + m*n)
          | m <- [1..perim]
          , n <- [1..perim `div` (2*m) - m]
          , k <- [1..perim `div` (2*(m*m + m*n))]
          , m > n
          , even n || even m
          , isCoprime m n
          ]

isCoprime a b
  | b == 0 = a == 1
  | otherwise = isCoprime b (a `mod` b)
