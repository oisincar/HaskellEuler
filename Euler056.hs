import Data.Char

main = print $ maximum [digSum (a^b) | a <- [1..100], b <- [2..100]]
  where digSum n = sum $ map digitToInt $ show n
