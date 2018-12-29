
-- In order to get last one being 0.. Last digit of root must be 0, and second last must be 3 or 7 to get 9
candidates :: [Int]
candidates = zipWith (+) (cycle [30, 70]) [st, st + 100..]
  where st = (floor $ sqrt 1020304050607080900) `div` 100 * 100

isAns n = checkSnd (show $ n*n) "1_2_3_4_5_6_7_8_9_0"
  where checkSnd (x1:_:xs) (y1:_:ys) = x1 == y1 && checkSnd xs ys
        checkSnd _ _ = True

main = print . head . filter isAns $ candidates
