import Data.List

main = print $ snd $ minimum $ [((3/7 - fromIntegral (nextLft x)/ (fromIntegral x), nextLft x))
                               | x <- [8..1000000], x `mod` 7 /= 0]
  where nextLft n = n*3 `div` 7
