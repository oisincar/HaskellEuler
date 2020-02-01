import Data.List

a, b, a_inv :: Integer
a = 1504170715041707
b = 4503599627370517
-- Modular inverse of a
a_inv = 3451657199285664

-- Sequence given
sq :: [Integer]
sq = a: (map (\x -> (x+a) `mod` b) sq)

-- Give strictly decreasing elements of a list.
decreasing :: [Integer] -> [Integer]
decreasing [] = []
decreasing (x:xs) = x: (decreasing $ dropWhile (>= x) xs)

main = do
  let bigCoins = decreasing $ take (10^8) sq
      bound = last bigCoins
      smallCoins = decreasing $ map snd $ sort [((x*a_inv) `mod` b, x) | x <- [1..bound-1]]
  print $ (sum bigCoins) + (sum smallCoins)
