main = print $ ans 100

ans n = sumList 0 0 !! n

sumList :: Integer -> Integer -> [Integer]
sumList lastN n = tot : sumList tot (n+1)
    where tot = lastN - n^2 + n^3
