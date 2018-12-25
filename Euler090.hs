import qualified Data.Set as S
import Data.List

-- Number pairs required for each square.
pairs :: [(Int, Int)]
pairs = [(ninetosix $ (a*a) `div` 10, ninetosix $ (a*a) `mod` 10) | a <- [1..9]]

ninetosix 9 = 6
ninetosix x = x

-- All sets with 6 elements put in em.. Although 6,9 are converted to the same (6)
all6Subsets :: [S.Set Int]
all6Subsets = allS' 9 6
  where
    -- Current Num -> # left -> all subsets of length 6
    allS' :: Int -> Int -> [S.Set Int]
    allS' x l
      | l == 0         = [S.empty]
      | x < 0 || l < 0 = []
      | otherwise      = allS' (x-1) l ++ (map (S.insert $ ninetosix x) (allS' (x-1) (l-1)))

lstSquare :: [a] -> [(a,a)]
lstSquare [] = []
lstSquare (a:as) = (map ((,) a) (a:as)) ++ lstSquare as

isValid :: S.Set Int -> S.Set Int -> Bool
isValid d1 d2 = and [(S.member a d1 && S.member b d2) || (S.member a d2 && S.member b d1) | (a,b) <- pairs]

validCombos = filter (uncurry isValid) (lstSquare all6Subsets)

main = print $ length validCombos
