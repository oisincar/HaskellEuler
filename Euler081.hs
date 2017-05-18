import qualified Data.MemoCombinators as Memo
import Debug.Trace

main = do
  file <- readFile "./p081_matrix.txt"
  let numbers = map (\s -> (read :: String -> [Integer]) ('[' : s ++ "]")) (lines file)
      sumToStart = Memo.memo2 (Memo.unsafeArrayRange (0,79))
                   (Memo.unsafeArrayRange (0,79)) (s numbers)

      s m x y | x == 0 && y == 0 = cPos
              | x == 0 = cPos + goUp
              | y == 0 = cPos + goLeft
              | otherwise = cPos + min goLeft goUp
        where cPos = m !! x !! y
              goUp = sumToStart x (y-1)
              goLeft = sumToStart (x-1) y

  print $ sumToStart 79 79
