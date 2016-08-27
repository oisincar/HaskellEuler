
import Data.List
import Data.Char

main = do
    names <- readFile "./p022_names.txt"
    let namesLst = sort $ (read :: String -> [String]) $ '[' : names ++ "]"
        namePairs = zip [1..] namesLst
        answer = sum $ map score namePairs
            where score (i,name) = i * (sum $ map (\x -> ord(x)-ord('A')+1) name)

    print $ answer
