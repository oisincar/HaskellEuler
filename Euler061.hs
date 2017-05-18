import Data.List
import Data.Maybe

main = print $ sum set

pn n = takeWhile (< 10000) $ dropWhile (< 1000) pn'
  where pn' = 1: zipWith (+) pn' [n-1, n-1 + n-2..]

nodes = map (\n -> (n, pn n)) [3..8]

set = head $ catMaybes [tryPerm graph x x | graph <- (permutations pns), x <- snd p]
  where (p:pns) = nodes

tryPerm :: [(Int, [Int])] -> Int -> Int -> Maybe [Int]
tryPerm [] fstN lstN
  | (lstN `mod` 100 == fstN `div` 100) = Just [lstN]
  | otherwise = Nothing

tryPerm (pn : pns) fstN lstN
  | recLst == [] = Nothing
  | otherwise = Just $ lstN : head recLst
  where recLst = catMaybes $ map (tryPerm pns fstN) nextNums
        nextNums = filter (\p -> lstN `mod` 100 == p `div` 100) (snd pn)
