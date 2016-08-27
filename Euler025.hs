main = print $ ans 1000

fibs = 1:1:zipWith (+) fibs (tail fibs)

lenFibs = map (length . show) fibs

ans n = (1+) $ length $ takeWhile (<n) lenFibs
