import qualified Data.Set as Set
import Debug.Trace

type Triple = (Integer, Integer)

baseTriples :: Integer -> [Triple]
baseTriples maxM = [(a, b) | n <- [1..maxM * 2], m <- [n+1..maxM * 2], let a = min (m*m - n*n) (2*m*n), let b = max (m*m - n*n) (2*m*n)]

revTriples m = filter (\(a,b) -> a*2 >= b) $ bT ++ [(b, a) | (a, b) <- bT, a /= b]
  where bT = baseTriples m

triples :: Integer -> [Triple]
triples m = Set.elems $ Set.fromList $ concat [takeWhile (\(a,b) -> a <= m && b <= (2*m)) [mul triple k | k <- [1..]] | triple <- (revTriples m)]
  where mul (a, b) k = (a*k, b*k)

tripleCombosL :: Triple -> [ (Integer, Integer, Integer) ]
tripleCombosL (a, b) = [(a, k, b-k) | k <- [(b+1) `div` 2.. min a (b-1)]]

tripleCombos :: Triple -> Integer
tripleCombos (a, b) = (min a (b-1)) - ((b+1) `div` 2) + 1
  -- (a, ??) .. (b/2 round up, b/2 round down)

k m = sum $ map tripleCombos (triples m)

-- binSearch min max _ _ | trace (show min ++ " " ++ show max) False = undefined
binSearch min max f goal
  | min > max   = min
  | f mid > goal = binSearch min (mid-1) f goal
  | otherwise    = binSearch (mid+1) max f goal
  where mid = (min + max) `div` 2

-- c = Set.elems $ Set.fromList $ concatMap tripleCombosL triples

main = print $ binSearch 0 5000 k 1000000
