import Data.List


  -- [(start ix, prime)]
truths = reverse $ zip [1..7] [2,3,5,7,11,13,17]

isDiv :: String -> Bool
isDiv str = all (\(ix, p) -> (numAt ix) `mod` p == 0) truths
  where numAt n = (read . take 3 . drop n) str

ans = filter isDiv $ permutations "0123456789"
main = print $ sum $ map (read :: String -> Int) ans
