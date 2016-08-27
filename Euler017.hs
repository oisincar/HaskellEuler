
main = print $ length $ concatMap writeNumber [1..1000]

writeNumber :: Int -> String
writeNumber n
    | n == 0 = ""
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | n == 10 = "ten"

    | n == 11 = "eleven"
    | n == 12 = "twelve"
    | n == 13 = "thirteen"
    | n == 14 = "fourteen"
    | n == 15 = "fifteen"
    | n == 16 = "sixteen"
    | n == 17 = "seventeen"
    | n == 18 = "eighteen"
    | n == 19 = "nineteen"

    | n == 20 = "twenty"
    | n < 30  = "twenty" ++ writeNumber (n-20)
    | n < 40  = "thirty" ++ writeNumber (n-30)
    | n < 50  = "forty" ++ writeNumber (n-40)
    | n < 60  = "fifty" ++ writeNumber (n-50)
    | n < 70  = "sixty" ++ writeNumber (n-60)
    | n < 80  = "seventy" ++ writeNumber (n-70)
    | n < 90  = "eighty" ++ writeNumber (n-80)
    | n < 100 = "ninety" ++ writeNumber (n-90)

    | n < 1000 = writeNumber (n`div`100) ++ "hundred" ++ andStr ++ writeNumber (n`mod`100)


    | n == 1000 = "onethousand"
    where andStr
            | n`mod`100 == 0 = ""
            | otherwise = "and"
