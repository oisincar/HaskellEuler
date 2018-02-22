import Data.Fixed
import Debug.Trace
import Data.List

-- Frac1   a/ (sqrt(b) + c)
data Frac1 = Frac1 Int Int Int deriving (Show, Eq)

-- Frac2   a*(sqrt(b) + c) / d
data Frac2 = Frac2 Int Int Int Int deriving (Show)

step1 :: Frac1 -> Frac2
step1 frac = Frac2 a b (-c) (b - c*c)
  where (Frac1 a b c) = frac


step2 frac = Frac1 den b (c - z*den)
  where (Frac2 a b c d) = frac
        den = (d `div` a)
        z = firstNum frac

firstNum :: Frac2 -> Int
firstNum (Frac2 a b c d) = round0 $ (a'*(sqrt(b') + c') / d')
  where a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c
        d' = fromIntegral d

fstFrac = Frac1 1 23 (-4)

round0 x
  | x > 0 = floor x
  | otherwise = ceiling x


fracN :: Int -> Frac1
fracN i = Frac1 1 i 1

findLoops :: [Frac1] -> Int
findLoops fs = loops fs []

loops :: [Frac1] -> [Frac1] -> Int
loops (f:fs) pastfs
  | f `elem` pastfs = 0
  | otherwise       = 1 + (loops fs (f:pastfs))

fracs :: Frac1 -> [Frac1]
fracs frac = iterate (step2 . step1) frac

loopsForN = findLoops . (drop 2) . fracs . fracN

nonSquares :: Int -> [Int]
nonSquares a = [1..a] \\ [x * x | x <- [1.. floor . sqrt $n]]
  where
    n :: Double
    n = fromIntegral a

ans = length $ filter odd $ map loopsForN $ nonSquares 10000

main = print ans
