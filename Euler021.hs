
main = print $ sum $ filter isValid [1..10000]

isValid num = (num == sumD2) && (num /= sumD)
    where sumD  = sumDivisors num
          sumD2 = sumDivisors sumD

sumDivisors :: Int -> Int
sumDivisors num = 1 + sum (filter (\x -> num `mod` x == 0) [2..(num `div` 2)])
