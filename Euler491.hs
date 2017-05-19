import Data.MemoCombinators
import Data.List

ans = (sum $ map ways numWays)

facts = 1: zipWith (*) facts [1..]

-- Ways to group the numbers (1-9)*2 so the difference is a multiple of 11.
numWays = map init $ groupingsM 0 0 0

ways :: [Int] -> Int
ways xs = (facts !! 10) * (facts !! 10) * (10 - (head numcounts)) `div` (combs (numcounts) * 10)
  where numcounts = map (\x -> length x -1) $ group $ sort $ xs ++ [0..9]

-- takes list of counts of a number
combs :: [Int] -> Int
combs ls = (combs' ls) * (combs' $ map (\x -> 2-x) ls)
  where combs' cs = (product $ map (facts !!) cs)

-- sum (mod 11) -> curr number -> depth (no. nums added already) -> count
groupingsM :: Int -> Int -> Int -> [[Int]]
groupingsM = memo3 integral integral integral groupings'
    where groupings' sum n d
            | d == 10 && sum == 1 = [[0]]
            | d == 10 || n == 10  = []
            | otherwise =         groupingsM (sum) (n+1) d
              ++ (map (n :)     $ groupingsM ((sum + n) `mod` 11) (n+1) (d+1))
              ++ (map ([n,n]++) $ groupingsM ((sum + 2*n) `mod` 11) (n+1) (d+2))
