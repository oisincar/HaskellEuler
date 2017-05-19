main = print $ length [x | i <- [1..9], x <- (takeWhile (\x -> i^x >= 10^(x-1)) [1..])]
