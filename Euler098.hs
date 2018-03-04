import Data.List
import Data.Tuple
import Data.List.Split

data Exponential = Exp Double Double Int

main = do
  file <- readFile "./p098_words.txt"

  let words = (read :: String-> [String]) $ "[" ++ file ++ "]"
      wordGroups = filter (\g -> length g > 1) $ map (map snd) $ groupBy (\(a,_) (b,_) -> a == b) $ sort $ map (\s -> (sort s, s)) words

      allGroups = concatMap mappingsInGroup wordGroups

  print $ maximum allGroups


mappingsInGroup :: [String] -> [Int]
mappingsInGroup [] = []
mappingsInGroup (s:ss) = concatMap (getMappings s) ss
                         ++ mappingsInGroup ss

getMappings :: String -> String -> [Int]
getMappings a b = concatMap (getOtherNum a b) squares
  where squares =
          takeWhile (\s -> s < 10^(length a)) $
          dropWhile (\s -> s < 10^(length a - 1)) [(s*s) | s <- [1..]]

-- If it's a mapping, both squares involved in the pairing. Otherwise empty list.
getOtherNum :: String -> String -> Int -> [Int]
getOtherNum word1 word2 num
  | isValidMapping
  && (head otherNumStr /= '0')
  && (isSquare otherNum) = [num, otherNum]
  | otherwise            = []
  where -- All mappings, removing duplicates.
        mapping = map head $ group $ sort $ zip word1 (show num)
        -- All letters must map to a single number.
        isValidMapping = (uniqueFunc mapping) && (uniqueFunc $ map swap mapping)
        uniqueFunc map = all (\m -> length m == 1) $ groupBy (\(a,_) (b,_) -> a == b) $ sort map
        -- And all numbers must map to a single
        otherNumStr = foldr (\(a, i) -> replace a i) word2 mapping
        otherNum = read otherNumStr

replace :: Char -> Char -> String -> String
replace a b [] = []
replace a b (s:ss)
  | a == s    = b : (replace a b ss)
  | otherwise = s : (replace a b ss)


isSquare :: Int -> Bool
isSquare n = truncate(sqrt(x)) * truncate(sqrt(x)) == n
             where x = fromIntegral n
