import Data.Char

conv k = 1:(2*k):1 : conv (k+1)

fe2 [] = (0,1)
fe2 (c:cs) = (b,(c*b + t))
  where (t,b) = fe2 cs

frac2 n = (t+2*b, b)
  where (t,b) = (fe2 $ take n $ conv 1)

main = print $ sum $ map digitToInt (show . fst . frac2 $ 99)
