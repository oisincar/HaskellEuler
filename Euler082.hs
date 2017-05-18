import qualified Data.MemoCombinators as Memo
import Debug.Trace

main = do
  file <- readFile "./p082_matrix.txt"
  let numbers = map (\s -> (read :: String -> [Integer]) ('[' : s ++ "]")) (lines file)

      sumToStart = Memo.memo2 (Memo.unsafeArrayRange (0,79))
                   (Memo.unsafeArrayRange (0,79)) (s numbers)

      s m x y | x == 0    = cPos
              | otherwise = minimum [(sumVals y yn) + (sumToStart (x-1) yn) | yn <- [0..79]]
        where cPos = m !! y !! x
              goUp = sumToStart x (y-1)
              goDown = sumToStart x (y+1)
              goLeft = sumToStart (x-1) y
              -- Much slower accessing secondary axis first.
              -- Should really rotate list after reading it.
              sumVals a b = sum $ map (!!x) $ drop (min a b) $ take ((max a b) +1) m

  print $ minimum $ map (sumToStart 79) [0..79]
