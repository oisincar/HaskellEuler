
main = print ans

ans = x*2 + 3
  where x = length $ takeWhile (\(a,b) -> a*10 > b) (rowPerc 3 0)

-- for given sidelength length n.
row n = take 3 [n*n - n + 1, n*n - 2*n + 2..]

rowPerc :: Int -> Int -> [(Int, Int)]
rowPerc n tot = (newTot, (n-1)*2 + 1) : (rowPerc (n+2) newTot)
  where newTot = tot + (length $ filter isPrime $ row n)

isPrime n = not $ any (\x -> n `mod` x == 0) (2:[3,5.. introot n])
  where introot = floor . sqrt . fromIntegral
