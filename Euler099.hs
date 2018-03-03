import Data.List
import Data.List.Split

data Exponential = Exp Double Double Int

main = do
  file <- readFile "./p099_base_exp.txt"
  let fracLists = map ((map read) . (splitOn ",")) $ lines file
      fracs = zipWith ((\[a,b] i -> Exp a b i)) fracLists [1..]

  print $ (\(Exp a b i) -> i) $ maximumBy compareExp fracs

compareExp :: Exponential -> Exponential -> Ordering
compareExp (Exp a b _) (Exp x y _) =
  compare b (y * logBase a x)
