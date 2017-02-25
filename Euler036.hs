isPal :: String -> Bool
isPal str
  | length str <= 1 = True
  | otherwise       = head str == last str && (isPal (drop 1 (take (length str - 1) str)))

isAns :: Int -> Bool
isAns = isPal . show

bin :: Int -> String
bin 0 = []
bin n | n `mod` 2 == 0 = bin (n `div` 2) ++ "0"
      | otherwise      = bin (n `div` 2) ++ "1"


ans = sum $ filter (\x -> (isPal.show) x && (isPal.bin) x) [1..1000000]

main = print ans
