import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
-- import Data.Set (Set)
-- import qualified Data.Set as Set
import Data.Function

primes = 2 : 3 : 5 : primes'
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

nDigPrimes numDig = takeWhile (< 10^numDig) $
                     dropWhile (< 10^(numDig-1)) primes

updateMap :: [[Int]] -> Int ->
             Map.Map (String, [Int]) [Int] ->
             Map.Map (String, [Int]) [Int]
updateMap (xr:xrs) p mapP
  | isEqualAtP (show p) xr =
      updateMap xrs p (inc xr mapP)
  | otherwise =
      updateMap xrs p mapP
    where
      isEqualAtP pStr key =
        let lst = map (\i -> pStr !! i) key
        in all (== head lst) $ tail lst
      inc key = Map.insertWith (++) (showP p key, key) ([p])
updateMap _ _ mapP = mapP

showP :: Int -> [Int] -> String
showP p key = foldr dropAt (show p) key
  where dropAt x str = (take x str) ++ (drop (x+1) str)

-- returns any valid primes for a given length
ansM c l = Map.toList $ Map.filter (\a -> length a >= c) (ansLst l)
  where ansLst l = foldr (updateMap $ xorLst l) Map.empty (nDigPrimes l)

main = do
  print p
    where (_,(p:ps)) = head $ concatMap (ansM 8) [1..]

xorLst x = concat $ take x $ f x
  where
    f :: Int -> [[[Int]]]
    f d = (map (\a -> [a]) [0..d-1]) : (map g (f d))

    g :: [[Int]] -> [[Int]]
    g x = concatMap h x

    h :: [Int] -> [[Int]]
    h x = map (\a -> a : x) [0..(head x)-1]
