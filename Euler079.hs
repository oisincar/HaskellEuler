import Data.List

main = do
  file <- readFile "/home/oisincar/DevCrap/HaskellSwag/HaskellEuler/p079_keylog.txt"
  let rules = map head . group . sort $ words file
  putStrLn $ head $ filter (isValid rules) posKeys

-- by looking at occurinces of letters in the keylog, only "01236789" are used, and occur in equal numbers.
-- make assumption that it's a simple permutation
posKeys = permutations "01236789"

isValid :: [String] -> String -> Bool
isValid rules str = all (\r -> obeysRule r str) rules
  where
    obeysRule [] _ = True
    obeysRule _ [] = False
    obeysRule (r:rs) (s:ss)
      | r == s    = obeysRule rs ss
      | otherwise = obeysRule (r:rs) ss
