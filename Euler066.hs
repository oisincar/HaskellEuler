import Data.List
import Data.Fixed
import Debug.Trace

-- Frac1   a/ (sqrt(b) + c)
data Frac1 = Frac1 Integer Integer Integer deriving (Show, Eq)

-- Frac2   a*(sqrt(b) + c) / d
data Frac2 = Frac2 Integer Integer Integer Integer deriving (Show)

step1 :: Frac1 -> Frac2
step1 (Frac1 a b c) = Frac2 a b (-c) (b - c*c)

step2 :: Frac2 -> (Integer, Frac1)
step2 (Frac2 a b c d) = (z, Frac1 den b (c - z*den))
  where den = (d `div` a)
        z = firstNum (Frac2 a b c d)

firstNum :: Frac2 -> Integer
firstNum (Frac2 a b c d) = round0 $ (a'*(sqrt(b') + c') / d')
  where a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c
        d' = fromIntegral d

fstFrac = Frac1 1 23 (-4)

round0 x
  | x > 0 = floor x
  | otherwise = ceiling x


fracN :: Integer -> (Integer, Frac1)
fracN i = (c, Frac1 1 i (-c))
  where c = floor . sqrt . fromIntegral $ i

fracs :: Frac1 -> [Frac1]
fracs frac = iterate (snd . step2 . step1) frac

nonSquares :: Integer -> [Integer]
nonSquares a = [1..a] \\ [x * x | x <- [1.. floor . sqrt $n]]
  where
    n :: Double
    n = fromIntegral a

-- Give the list of a0;a1..an for the fraction representation of the root of the given number.
continouedFraction :: Integer -> [Integer]
continouedFraction i = a0 : as
  where
    (a0, frac0) = fracN i
    as = map (fst . step2 . step1) . fracs $ frac0

convergents = convergents' . continouedFraction
-- Sequence of convergents from the above
convergents' :: [Integer] -> [(Integer, Integer)]
convergents' (a0:a1:as) = recurse t0 t1 as
  where
    t0 = (a0, 1)
    t1 = (a0*a1 + 1, a1)
    -- "t n-2  t n-1"
    recurse (hn2, kn2) (hn1, kn1) (an:as) = (hn2, kn2) : recurse (hn1, kn1) (hn, kn) as
      where hn = an * hn1 + hn2
            kn = an * kn1 + kn2

-- Solve any pell equation (fundemental solution)!!!!! x^2 - d*y^2 = 1
solve :: Integer -> (Integer, Integer)
solve d = head $ [(x,y) | (x,y) <- convergents d, x*x - d*y*y == 1]


-- (x,d) for all minimal pell equations.
xd = [(fst . solve $ d, d) | d <- [1..1000] \\ [x*x | x <- [1.. floor $ sqrt 1000 ]]]

main = print $ maximum xd
