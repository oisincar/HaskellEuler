import Data.Graph.Inductive
import Data.Graph.Inductive.Query

main = do
  file <- readFile "/home/oisincar/DevCrap/HaskellSwag/HaskellEuler/p083_matrix.txt"
  let nums = map (\s -> (read :: String -> [Int]) ("[" ++ s ++ "]")) $ lines file

  let dim = 80
      size = dim*dim

      nodes = zip [0..] (concat nums)

      edges = [(x1 + y1*dim, x2 + y2*dim, snd (nodes !! (x2 + y2*dim)))
              | x1 <- [0..dim-1], y1 <- [0..dim-1]
              , (x2,y2) <- [(x1+1,y1), (x1,y1+1), (x1-1,y1), (x1,y1-1)]
              , x2 >= 0, x2 < dim, y2 >= 0, y2 < dim
              ]

  let gr = mkGraph nodes edges :: Gr Int Int

  print $ (snd . head $ nodes) + spLength 0 (size-1) gr
