

main = print $ totalTris 50

totalTris bound = (trisAlongAxis bound) + (otherTris bound)

-- Tris with right angle along an axis
trisAlongAxis bound = bound*bound * 3

-- Other ones...
otherTris bound = sum $ map (length . (trisFrom bound)) [(x,y) | x <- [1..bound], y <- [1..bound]]

-- Y = mX + c, m = -a/b
-- Y = -a/bX + c
-- b = -a*a/b + c
-- b + a*a/b = c
-- "y = " ++ (show m) ++ "x + " ++ show c
-- y = -a/b*x + a*a/b + b
-- 0 = x + (-b/a)(a*a/b) + b(-b/a)
-- x = a + b*b/a

-- c = x1*x1/y1 + y1

trisFrom :: Int -> (Int, Int) -> [(Int, Int)]
trisFrom bound (x1,y1) = tris
  where
    -- m = - fromIntegral x1/y1
    -- Increment of x that'll keep y being an integer, is also an integer
    incX = y1 `div` (gcdE x1 y1)
    -- incY = -(incX * x1) `div` y1
    -- minimum value of x that's still a triangle
    minX :: Int
    minX = x1 `mod` incX
    maxX :: Int
    maxX = x1 + ((y1*y1) `div` x1)

    tris :: [(Int, Int)]
    tris = [(a,b) | a <- [minX, minX + incX ..maxX], let b = (-a*x1 + x1*x1) `div` y1 + y1, a /= x1, a <= bound, b <= bound] -- b = ma + c, but smart integer division.


-- proof!
-- a = and [gcd a b == gcdE a b | a <- [1..1000], b <- [1..1000]]
gcdE a b
  | a < b    = gcdE b a
  | r == 0   = b
  | otherwise = gcdE b r
    where r = a `mod` b
          q = a `div` b


