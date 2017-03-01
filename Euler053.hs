
fact = 1: zipWith (*) fact [1..]

ch fact n r = (fact !! n) `div` ((fact !! r) * (fact !! (n-r)))

main = print $ length [(n,r) | n <- [1..100], r <- [1..n-1], ch fact n r > 1000000]
