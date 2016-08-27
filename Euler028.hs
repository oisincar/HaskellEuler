
main = print $ sumLayers 1001
  where sumLayers n = 1 + sum (map layerN [3,5..n])
        layerN n = n*n*4 - 6*(n-1)
