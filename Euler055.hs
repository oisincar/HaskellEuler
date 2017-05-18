
isLychrel n = isLRec n 50
  where isLRec n c
          | c == 0 = True
          | otherwise = (not . isPal $ step n) && isLRec (step n) (c-1)

isPal n = (show n) == (reverse . show $ n)

step :: Integer -> Integer
step n = n + (read . reverse . show $ n)

main = print $ length $ filter isLychrel [1..10000-1]
