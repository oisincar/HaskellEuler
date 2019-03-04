import Debug.Trace
import Text.Printf

main = printf "%.6f\n" $ (solve [1,0,0,0,0] 15)-1

-- Take the paper at index i
takePaper :: [Double] -> Int -> [Double]
takePaper ps i = zipWith (+) ps ((replicate i 0) ++ [-1] ++ [1,1..])

solve :: [Double] -> Int -> Double
-- solve env i | trace ("solve " ++ show env ++ " " ++ show i) False = undefined
solve env i
  | i <= 1    = this
  | otherwise = this + sum [p * solve (takePaper env ix) (i-1)
                           | (p, ix) <- zip env [0..], p > 0] / (sum env)
    where this = if sum env == 1 then 1 else 0
