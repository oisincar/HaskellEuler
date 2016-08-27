import           Data.List

main = print $ ans 1000
  where ans = snd . maximum . pairs

largeNum = 10^10000

-- testing a length
isLoopN :: [Char] -> Int -> Bool
isLoopN str num = isL num str (take num str)
    where isL num list seq
            | length list < num = True
            | not (isPrefixOf seq list) = False
            | otherwise = isL num (drop num list) seq

lenLoop :: [Char] -> Int
lenLoop str = 1+ (length $ takeWhile (== False) (map (isLoopN (drop 1000 str)) [1..1000]))

pairs n = map (\x -> (lenLoop $ show (largeNum `div` x), x)) [1..n]
