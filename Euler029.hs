import           Data.List

main = print ans
  where ans = length $ nub answerList

answerList = map (uncurry (^)) pairs
  where pairs = concatMap (zip [2..100] . repeat) [2..100]
