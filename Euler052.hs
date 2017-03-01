import Data.List

try n = filter isValid nums
  where nums = [10^n..(10^(n+1) `div` 6)]
        isValid n =
          let lst = map (\t -> sort . show $ n*t) [1..6]
          in all (== head lst) $ tail lst

main = print $ head $ concatMap try [1..]
