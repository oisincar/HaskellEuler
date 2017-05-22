import Data.Array

-- Same as 78.hs

-- Discount the full group of 100, only sums allowed.
main = print $ (ans 100) -1

pents = 1: zipWith (+) pents (merge [1..] [3,5..])
  where merge (x:xs) (y:ys) = x:y: merge xs ys

ans :: Int -> Integer
ans n = r!n
  where r = listArray (0, 100000) (1: map sumP [1..])
        sumP n = b $ map (n-) pents

b (p1:p2:ps)
  | p2 >= 0 = ans p1 + ans p2 - b ps
  | p1 >= 0 = ans p1
  | otherwise = 0
