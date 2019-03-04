import Data.List.Split

main = do
  file <- readFile "./p102_triangles.txt"
  let triangles :: [[Int]]
      triangles = map (map read . splitOn ",") $ lines file

  print $ length $ filter contsOrigin triangles

-- Tri contains a point if all sides are to the same 'side' of it.
contsOrigin (x1:y1:x2:y2:x3:y3:_) = s1 == s2 && s1 == s3
  where s1 = originSide (x1,y1) (x2,y2)
        s2 = originSide (x3,y3) (x1,y1)
        s3 = originSide (x2,y2) (x3,y3)

-- Dot product z dir
originSide (x1,y1) (x2,y2) = signum $ x1*(y2-y1) - y1*(x2-x1)
