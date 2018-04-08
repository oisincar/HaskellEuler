-- Equation for the number of total balls of the nth valid combination.
-- Courtisy of wolfram alpha.
-- (Rearranged basic probability formula into the form of pell's equation, and solved.)
total :: Double -> Double
total n = 1/4*((2*sqrt(2)-3)**n - sqrt(2)*(3 - 2*sqrt(2))**n - (3 + 2*sqrt(2))**n + sqrt(2)*(3 + 2*sqrt(2))**n + 2)

-- Find blue balls given total.
-- (b*(b-1)) / (t*(t-1)) = 1/2
-- b(b-1)     = t*(t-1)/2
-- b^2 - b - 1/4 = t*(t-1)/2 - 1/4
-- (b-1/2)^2 = t*(t-1)/2 - 1/4
-- b = sqrt(t*(t-1)/2 - 1/4) + 1/2
blue t = sqrt (t*(t-1)/2 - 1/4) + 1/2

main = do
  let numTotal = head . dropWhile (<10**12) $ map total [1..100]
      numBlue  = round $ blue numTotal
  print numBlue
