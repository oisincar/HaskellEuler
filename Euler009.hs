import           Data.List
import           Data.Maybe

main = print $ testCase pyTriples 1000
  where pairs = concatMap (\x -> zip (repeat x) [x..1000]) [1..1000]
        triples = map (\(x,y) -> (x, y, pythag x y)) pairs
        pyTriples = filter isTriple triples
        pythag x y = sqrt $ fromIntegral (x*x + y*y)

testCase :: [(Int, Int, Float)] -> Int -> Int
testCase pyTrs n = times $ fromJust $ find (\(a,b,c) -> a+b+floor c == n) pyTrs
    where times (a,b,c) = a*b* floor c

isTriple :: (Int, Int, Float) -> Bool
isTriple (x,y,z) = floor z == ceiling z
