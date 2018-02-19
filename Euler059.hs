import Data.Ord
import Data.Bits
import Data.Char
import Data.List

main = do
  file <- readFile "./p059_cipher.txt"
  let numbers = (read :: String -> [Int]) ("[" ++ file ++ "]")
  let msg = combine
            (snd $ letter2 $ take3rd numbers)
            (snd $ letter2 $ take3rd $ drop 1 numbers)
            (snd $ letter2 $ take3rd $ drop 2 numbers)

  print $ sum $ map ord msg

combine (x:xs) (y:ys) (z:zs) = x:y:z : combine xs ys zs
combine (x:xs) (y:ys) _ = x:y:[]
combine (x:xs) _ _ = x:[]

take3rd (x:_:_:xs) = x: take3rd xs
take3rd (x:_) = x:[]
take3rd _ = []

-- letter :: [Int] -> Char
letter2 xs = maximumBy (comparing numChars) $ map xorC [ord 'a'.. ord 'z']
  where xorC c = (c, map (\x -> chr (x `xor` c)) xs)
        numChars (_,xs) = length $ filter (\c -> c > 'A' && c < 'z' || c == ' ') xs
