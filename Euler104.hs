import Data.List
import Data.Maybe

-- main = print $ fromJust $ findIndex (\(b,t) -> isPanDig b && isPanDig t) fibsStr
-- main = print $ fromJust $ findIndex (\(t,b) -> isPanDig b) fibsStr
main = print . (+1) . fromJust $ findIndex (\(b,t) -> isPanDig b && isPanDig t) fibsStr

isPanDig i = (sort . (take 9) . show $i) == "123456789"

fibsStr :: [(Integer, Integer)]
fibsStr = zip fbBot fbTop
  where fbBot = 1:1:(zipWith (\a b -> ((a+b) `mod` (10^9))) fbBot $ tail fbBot)
        fbTop = 1:1:(zipWith (+) fbTop $ tail fbTop)
