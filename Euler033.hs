

cancelDiag n d = (drop 1 )

cancelStart n = (read::String->Int) $ drop 1 $ show n
cancelEnd   n = (read::String->Int) $ take (length str - 1) $ str
  where str = show n

check (n,d) = (n < d) && diagCancel && ((notDup n) || (notDup d))
-- check (n,d) = diagCancel && ((notDup n) || (notDup d))
  where
    notDup x = (show x !! 0) /= (show x !! 1)
    diagCancel = (show d !! 0) == (show n !! 1) && (n * (cancelStart d)) == (d * (cancelEnd n)) ||
                 (show d !! 1) == (show n !! 0) && (n * (cancelEnd d)) == (d * (cancelStart n))

combs = [(x,y) | x <- [10..100], y <- [10..100]]

fracts = filter check combs

ans = foldr (\(a,b) (c,d) -> (a*c, b*d)) (1,1) fracts

main = print (b `div` a)
  where (a,b) = ans
