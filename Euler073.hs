import Data.List

main = print $ length $ takeWhile (< 1/2) $ dropWhile (<= 1/3) lstFracts
  where lstFracts = map head $ (group . sort) [a/b | b <- [1..12000], a <- [1..b]]
