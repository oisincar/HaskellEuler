main = print $ ans 10001

ans n = primes !! (n-1)
  where primes = 2 : primes'
        isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 ..]
