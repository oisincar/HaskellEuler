main = print ans

ans = length $ filter topLonger $ take 1000 $ iterate ittFrac (1,1)
  where topLonger (a,b) = (length . show) a > (length . show) b
        ittFrac (t,b) = (t + 2*b, b + t)
