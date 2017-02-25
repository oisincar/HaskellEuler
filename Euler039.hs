import Data.List
import Data.Ord

perims = filter (<= 1000)
         [(x + y + floor (pythag x y)) |
              x <- [1..500], y <- [1..500], isTriple (x,y,pythag x y)]
  where pythag x y = sqrt $ fromIntegral (x*x + y*y)
        isTriple (x,y,z) = floor z == ceiling z

ans = (head . head . sortBy (flip $ comparing length) . group . sort) perims

main = print ans
