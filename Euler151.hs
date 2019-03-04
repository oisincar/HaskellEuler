import Debug.Trace
import Text.Printf

main = printf "%.6f\n" $ (solve start 15)-1

-- 1*a1, 0*a2-5
start :: [Int]
start = [1,0,0,0,0]

-- Take the paper at index i
batch :: [Int] -> Int -> [Int]
batch ps i = zipWith (+) ps ((replicate i 0) ++ [-1] ++ [1,1..])

solve :: [Int] -> Int -> Double
-- solve env i | trace ("solve " ++ show env ++ " " ++ show i) False = undefined
solve env i
  | i <= 1    = this
  | otherwise = this + sum [(fromIntegral $ env !! ix) * solve (batch env ix) (i-1)
                           | ix <- [0..4], env !! ix /= 0] / (fromIntegral $ sum env)
    where this = if sum env == 1 then 1 else 0
