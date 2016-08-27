import           Data.List

main = putStrLn $ sort (permutations "0123456789") !! 999999
