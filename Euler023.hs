
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

main = print $ sum $ filter (not.isValid) [1..28123]

isValid num = isVal num abundantNumsLst abundantNumsSet
    where isVal :: IntSet.Key -> [IntSet.Key] -> IntSet -> Bool
          isVal num (x:xs) abNums
              | x > num = False
              | IntSet.member (num-x) abNums = True
              | otherwise = isVal num xs abNums

abundantNumsSet = IntSet.fromAscList abundantNumsLst
abundantNumsLst = filter (\x -> x < sumDivisors x) [1..28123]
    where sumDivisors num = 1 + sum (filter (\x -> num `mod` x == 0) [2..(num `div` 2)])
