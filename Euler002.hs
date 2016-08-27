main :: IO ()
main = print $ ans 4000000

ans :: Integer -> Integer
ans num = sum $ takeWhile (<num) fibs
  where fibs = 0:2:zipWith (\a b -> a + b*4) fibs (tail fibs)

{-
1 2 3 5 8 13 21 34
  |     |       |
1 2 (1+2) (1+1+2) (1+1+1+2+2)
0  1  1  2  3  5  8  13  21  34  55  89  144  233  377  610  987
|        |        |          |            |              |
-}
