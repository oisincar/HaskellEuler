import Data.List

main = print . length . map head . group . sort $[a/b | a <- [1..12000], b <- [a*2+1.. min (a*3-1) (12000)]]
