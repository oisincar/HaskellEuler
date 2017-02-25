import Data.List

nums x = filter (\s -> sort s == "123456789") $ map (panDig x) [1..9999]
  where panDig x n = concatMap (\x -> (show (x*n))) [1..x]

main = putStrLn $ last $ sort $ concatMap nums [2..8]
