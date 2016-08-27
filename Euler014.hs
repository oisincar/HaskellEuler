import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Maybe

main = print $ snd $ maximum $ loopR 1 IntMap.empty

loopR :: Int -> IntMap Int -> [(Int, Int)]
loopR 1000000 _ = []
loopR i map = (col, i) : loopR (i+1) (IntMap.insert i col map)
    where col = collatz i map

collatz :: Int -> IntMap Int -> Int
collatz i map
    | i == 1 = 1
    | isJust lookup = fromJust lookup
    | otherwise = 1 + collatz (step i) map
    where lookup = IntMap.lookup i map

step i
    | i `mod` 2 == 0 = i `div` 2
    | otherwise      = i*3 + 1
