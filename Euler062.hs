import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map



main = print $ pow^3
  where pow = head . snd . head $ lstNums
        lstNums = Map.toList $ head $ filter (not . Map.null) $ map ansMap d

ansMap = Map.filter (\ints -> length ints >= 5)

d = map (c . cubes) [1..]

c :: [(Int, String)] -> Map.Map String [Int]
c cbs = foldr (\(n,k) -> Map.insertWith (++) k [n]) Map.empty cbs

cubes :: Int -> [(Int, String)]
cubes p = takeWhile (\(n,_) -> n^3 < 10^p)
        $ dropWhile (\(n,_) -> n^3 < 10^(p-1))
        $ [(x, (sort.show) (x^3)) | x <- [1..]]
