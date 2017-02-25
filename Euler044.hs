import Data.Set (Set)
import qualified Data.Set as Set


diff n k = (3*k*k + 6*n*k - k) `div` 2

-- isPent n = n == (last $ takeWhile (<= n) pents)


boundedAns = head [p1-p2 | p1 <- pents, p2 <- takeWhile (< p1) pents,
                   isPent (p1-p2), isPent (p1+p2)]
  where isPent n = Set.member n pentSet
        pentSet = Set.fromList pents
        pents = take 4000 $ 1 : zipWith (+) pents [4,7..]

main = print boundedAns

-- Results in a single pair.. (p1020, p(1020 + 1147)) which has a difference of 5482660
-- This turns out to be the answer.. But we check to optimise it below.

-- Code to prove this one soloution is optimal...

-- gives max k for given n and difference
-- kForDiff :: Int -> Int -> Int
-- kForDiff ni di = ceiling $ 1/6 * (1 - 6*n + sqrt(1 + 24*d - 12*n + 36*n^2))
--   where n = fromIntegral ni
--         d = fromIntegral di

-- ans = [(diff n k, n, k) | n <- [1..1000000], k <- [1.. kForDiff n 5482660],
--        isPent (diff n k),
--        isPent (pents !! (n-1) + pents !! (n+k-1))]
--   where isPent n = Set.member n pentSet
--         pentSet = Set.fromList $ take 5000 pents
