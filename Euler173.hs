-- Minimum size for cutout square.
minInner :: Int -> Int -> Int
minInner t o = ceiling . sqrt . fromIntegral $ max (o*o - t) 1

count t o = (o - (minInner t o)) `div` 2

main = print $ sum [count t n | n <- [1..t]]
  where t = 1000000
