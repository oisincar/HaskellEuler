import Data.List
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

scoreWord :: String -> Int
scoreWord str = sum $ map (\c -> ord(c) - ord('A') + 1) str

main = do
    file <- readFile "./p042_words.txt"

    let words = (read :: String -> [String]) $ '[' : file ++ "]"
        scores = map scoreWord words

        triNums = map (\n -> n*(n+1) `div` 2) [1..]
        triSet = Set.fromList $ takeWhile (<= maximum scores) triNums

    print $ length $ filter (\n -> Set.member n triSet) scores
