import           Data.List

main = print ans

ans = palProds !! 0
  where palProds = reverse $ sort $ filter isPalendrome multiples
        multiples = concatMap (\x -> map (*x) [x.. 999]) [100.. 999]
        isPalendrome num = (numStr == reverse numStr) && (length numStr == 6)
          where numStr = show num
