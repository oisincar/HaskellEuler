import           Data.List

canditates :: Int -> [(Int, Int)]
canditates a = zip (repeat a) [a..maxPair]
  where len = length (show a)
        maxPair
          | len == 1  = 9999
          | otherwise = 999

allPos = map (\(a,b) -> a*b) $ filter (isPan) $ concatMap canditates [1..99]

uniquePos = map (\a -> head a) $ (group . sort) allPos

isPan :: (Int, Int) -> Bool
isPan (a,b) = length (str) == 9 &&
              not (any (\c -> c == '0') str) &&
              length (group (sort str)) == 9
  where str = show a ++ show b ++ show (a*b)

main = print $ sum uniquePos
