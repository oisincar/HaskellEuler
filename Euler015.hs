main = print $ ans 20
  where ans n = factorial (2*n) `div` factorial n ^2

factorial 0 = 1
factorial n = n * factorial (n-1)
