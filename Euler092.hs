import Data.Char

main = print $ length $ filter id $ map res [1..10000000]

res 1  = False
res 89 = True
res n  = res (step n)

step n = sum $ map ((^2) . digitToInt) (show n)
