 -- Num colours you already have -> num balls already chosen -> num ways to choose from here * num balls you'll have.
recursiveSol :: Integer -> Integer -> Integer
recursiveSol colours balls
  | balls == 20  = colours
  | colours == 7 = oldColour
  | otherwise    = newColour + oldColour
    where newColour = numNewColour * recursiveSol (colours+1) (balls+1) -- Combinations taking new colour.
          oldColour = numOldColour * recursiveSol (colours)   (balls+1) -- Combinations taking same colour.
          numNewColour = 70 - colours*10    -- # balls with colour not seen before.
          numOldColour = colours*10 - balls -- # balls with colour we have seen before.

main = do
  print ans
  where sumColours = recursiveSol 0 0
        totalWays = product [51..70] -- Ways to pick 20 objects from list.
        ans = (fromIntegral sumColours) / (fromIntegral totalWays)
