tris  = 1 : zipWith (+) tris  [2..]
pents = 1 : zipWith (+) pents [4,7..]
hexs  = 1 : zipWith (+) hexs  [5,9..]

ans = search tris pents hexs

search (t:ts) (p:ps) (h:hs)
  | t == p && t == h = t : search ts ps hs
  | minNum == t = search ts (p:ps) (h:hs)
  | minNum == p = search (t:ts) ps (h:hs)
  | minNum == h = search (t:ts) (p:ps) hs
    where minNum = minimum [t,p,h]

main = print $ ans !! 2
