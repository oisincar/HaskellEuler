import Data.List
import Control.Monad.CSP
import Data.List.Split

main = do
  fLines <- lines <$> readFile "/home/oisincar/DevCrap/HaskellSwag/HaskellEuler/p096_sudoku.txt"
  let sudukos = map (drop 1) $ chunksOf 10 fLines
      sudukoLsts = map (map (map (\c -> read (c:[])) )) sudukos
      answer = sum $ map (read . (concatMap show) . (take 3) . head . solveSudoku) sudukoLsts
  print $ answer

solveSudoku :: (Enum a, Eq a, Num a) => [[a]] -> [[a]]
solveSudoku puzzle = oneCSPSolution $ do
  dvs <- mapM (mapM (\a -> mkDV $ if a == 0 then [1 .. 9] else [a])) puzzle
  mapM_ assertRowConstraints dvs
  mapM_ assertRowConstraints $ transpose dvs
  sequence_ [assertSquareConstraints dvs x y | x <- [0,3,6], y <- [0,3,6]]
  return dvs
      where assertRowConstraints =  mapAllPairsM_ (constraint2 (/=))
            assertSquareConstraints dvs i j =
                mapAllPairsM_ (constraint2 (/=)) [(dvs !! x) !! y | x <- [i..i+2], y <- [j..j+2]]

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ f []     = return ()
mapAllPairsM_ f (_:[]) = return ()
mapAllPairsM_ f (a:l) = mapM_ (f a) l >> mapAllPairsM_ f l
