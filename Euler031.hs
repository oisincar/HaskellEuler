
main = print $ combinations 200

coins = [1,2,5,10,20,50,100,200]

combinations :: Int -> Int
combinations = combis coins
    where combis coinsLft moneyLeft
            | moneyLeft == 0 = 1
            -- moneyLeft < 0  = error "Oops"
            | otherwise = layer coinsLft moneyLeft
            where
                layer (c:cs) moneyLeft
                  | monLft   >= 0 = combis (c:cs) monLft + layer cs moneyLeft
                  | otherwise = 0
                  where monLft = moneyLeft - c
                layer _ moneyLeft = 0
