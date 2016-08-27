main = print $ ans 600851475143
  where ans = tstCase (2 : [3,5..]) 1

tstCase :: [Integer] -> Integer -> Integer -> Integer
tstCase (x:xs) maxD num
    | x > squrt = max maxD num        -- if we've reached over sqrt with no divider, num must be prime
    | modN == 0 = tstCase (x:xs) x (num `div` x) -- try same number again.
    | otherwise = tstCase xs maxD num -- doesn't divide, move on
    where modN = num `mod` x
          squrt = floor $ sqrt $ fromIntegral num
