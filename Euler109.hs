singles = [1..20] ++ [25]
doubles = map (*2) singles
triples = [3,6..60]

ans = (ways b 1) + (ways b 2) + (ways b 3)
  where b = 100

ways bound throws
  | throws == 0 = if (bound <= 0) then 0 else 1
  | throws == 1 = sum [ways (bound-n) 0 | n <- doubles] -- Only throw doubles
  | throws == 2 = sum [ways (bound-n) 1 | n <- (singles ++ doubles ++ triples)]
  | throws == 3 = sum [ways (bound-n) 1 | n <- allPairs (singles ++ doubles ++ triples)]

allPairs [] = []
allPairs (x:xs) = (map (+x) (x:xs)) ++ allPairs xs
