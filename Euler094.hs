
baseATriples perim = [(a,b,c)
            | n <- [1.. (sqrtI perim)]
            , m <- [n.. (sqrtI perim) `div` 2]
            , even n || even m
            , let a = m*m - n*n
                  b = 2*m*n
                  c = m*m + n*n
            , a*2 == c+1 || a*2 == c-1 || b*2 == c+1 || b*2 == c-1
            , isCoprime m n
            ]

-- triPerims perim = [perimTri (a,b,c) | (a,b,c) <- baseATriples perim, a+b+c <= perim]
triPerims perim = filter (<= perim) $ map perimTri $ baseATriples perim

-- Change in sum perimiter for a triple.
-- This is a fking dumb way to do it too.
perimTri (a,b,c)
 | a /= b    = leftPerim (a,b,c) + leftPerim (b,a,c)
 | otherwise = leftPerim (a,b,c)
  where leftPerim (a,b,c)
          | a*2 == c+1 || a*2 == c-1 = 2*(a+c)
          | otherwise                = 0

main = print $ sum $ triPerims 1000000000

sqrtI = round . sqrt . fromIntegral

isCoprime a b
  | b == 0 = a == 1
  | otherwise = isCoprime b (a `mod` b)

-- Can unfold over a or b.
-- where a = m*m - n*n
--       b = 2*m*n
--       c = m*m + n*n
--      ^
--     /|\
--    / | \
--   c  a  \
--  /   | A \
-- /_ b_|____\
-- \    |
--  \ B |
--   \  |
--    \ |
--     \|
--      v

-- A type only valid if 2*b +-1 = c
                  -- if 4*m*n +- 1 = m*m + n*n
                  --    m*m + n*n - 4*m*n = +- 1
-- perim = 4*m*n + 2*(m*m + n*n)
-- or

-- B type only valid if 2*a +-1 = c
                  -- if 2*m*m - 2*n*n +-1 = m*m + n*n
                  --    m*m - 3*n*n = +-1
                  -- Seems to be no solutions for -1..

-- perim = 2*(m*m - n*n) + m*m + n*n
--       = 3*m*m - n*n
