import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

edges = 5
n = edges*2

a s = [[a,b,c] | a <- [1..n], b <- [a+1..n], c <- [b+1..n], a+b+c == s]

search :: [[Int]] -> Int -> Set.Set [Int] -> Int -> [[Int]]
search graph fstN visited lstN
  | Set.size visited == edges
    && fstN == lstN = [[lstN]]
  | nextEdges == []         = []
  | otherwise = concatMap visitN nextEdges
  where
    visitN :: [Int] -> [[Int]]
    visitN edge = (map ([a,lstN,b] ++) (search graph fstN(Set.insert edge visited) b))
               ++ (map ([b,lstN,a] ++) (search graph fstN(Set.insert edge visited) a))
      where (a:b:_) = filter (/= lstN) edge
    nextEdges = filter (\xs -> not (Set.member xs visited)
                                   && lstN `elem` xs) graph

main = putStrLn $ concatMap show bstRing
  where bstRing = last . sort $ y

y = filter isValid $ concat [z (a s) n | s <- [9..20], n <- [1..n]]

z edges n = map init $ search edges n Set.empty n

isValid xs =
  -- No more than 2 of any number
  (all (\l -> length l <= 2) grouped)
  -- 10 only occurs once
  && ((head . head) grouped /= 10 || (length . head) grouped == 1)
  -- All numbers present
  && (length grouped == n)
  -- No repeats in extremities
  && ((length . group) extremities == edges)
  -- First is largest extremity
  && (head xs == head extremities)
  where grouped = (group . reverse . sort) xs
        extremities = sort (everyN 3 xs)

everyN n = map head . takeWhile (not . null) . iterate (drop n)
