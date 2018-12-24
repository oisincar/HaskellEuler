import Data.List

data Board = B [Double] deriving (Show)

maxRoll = 4
boardSize :: Int
boardSize = 40

stBoard = B $ 1.0 : replicate (boardSize -1) 0
-- stBoard = B $ replicate 40 (1.0/ (fromIntegral boardSize))

probabilityRoll :: [(Int, Double)] -- [ (Roll, probability of roll) ]
probabilityRoll = [(head grls, (len grls) / (len rolls)) | grls <- group rolls]
  where
    rolls = sort [a + b | a <- [1..maxRoll], b <- [1..maxRoll]]
    len = fromIntegral . length

prob3rdDouble :: Double
prob3rdDouble = 1/(fromIntegral maxRoll**3)


update :: Board -> Board
update = normalize . applySpecials . applyRoll

normalize :: Board -> Board
normalize (B spaces) = B $ map (/mean) spaces
  where mean = (sum spaces)

applySpecials :: Board -> Board
applySpecials (B spaces) = B $ map update [0..boardSize]
  where
    -- prob of specific card from community chest/ chance
    probCC = (1.0/16.0) * (getIx 2 + getIx 17 + getIx 33)
    probCH = (1.0/16.0) * (getIx 7 + getIx 22 + getIx 36)

    update :: Int -> Double
    update ix = ((getIx ix) + moveTo ix) * moveFrom ix

    -- Go
    moveTo 0 = probCC + probCH

    -- Jail
    moveTo 10 = prob3rdDouble
             -- G2J
             + getIx 30
             + probCC
             + probCH

    -- C1
    moveTo 11 = probCH
    -- E3
    moveTo 24 = probCH
    -- H2
    moveTo 39 = probCH

    -- R1
    moveTo 5 = probCH
               -- Next railway
               + (2.0/16.0) * getIx 36
    moveTo 15 = (2.0/16.0) * getIx 7   -- R2
    moveTo 25 = (2.0/16.0) * getIx 22  -- R3
    -- (R4 isn't the next railway ever)

    -- Next utility (CH)
    moveTo 12 = (1.0/16.0) * getIx 7  -- U1
              + (1.0/16.0) * getIx 36
    moveTo 28 = (1.0/16.0) * getIx 22 -- U2

    -- Go back 3 (CH)
    moveTo 4  = (1.0/16.0) * getIx 7
    moveTo 19 = (1.0/16.0) * getIx 22
    moveTo 33 = (1.0/16.0) * getIx 36

    -- No bonuses for movement here...
    moveTo _ = 0

    -- Chance you stay on this square after landing here
    moveFrom :: Int -> Double
    moveFrom ix
      -- G2J
      | ix == 30 = 0
      -- CC
      | ix == 2 || ix == 17 || ix == 33 = (14.0/16.0)
      -- CH
      | ix == 7 || ix == 22 || ix == 36 = (4.0/16.0)
      | otherwise = 1

    -- 1-prob3rd roll chance we just go to jail...
    -- updateStd i = getIx i

    getIx :: Int -> Double
    getIx i = spaces !! (i `posMod` boardSize)

applyRoll :: Board -> Board
applyRoll (B spaces) = B $ map update [0..boardSize]
  where
    update :: Int -> Double
    update ix = (sum $ map (\(off, prob) -> prob * getIx (ix - off)) probabilityRoll)  * (1-prob3rdDouble)

    getIx :: Int -> Double
    getIx i = spaces !! (i `posMod` boardSize)

posMod a b = (b + (a `mod` b)) `mod` b


mostLikely = reverse . sort $ zip spaces [0..]
  where
    (B spaces) = (iterate update stBoard) !! 1000

main = putStrLn . concatMap show $ take 3 $ map snd mostLikely
