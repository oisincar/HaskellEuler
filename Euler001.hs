main = print $ ans (1000-1)

ans :: Int -> Int
ans n = 3*sumSeries (n `div` 3)
      + 5*sumSeries (n `div` 5)
      - 15*sumSeries (n `div` 15)
  where sumSeries n = (n*n + n) `div` 2
