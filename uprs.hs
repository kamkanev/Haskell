repStr str n = 
    if n == 0
        then ""
    else str ++ (repStr str (n-1))

triangle n = 
    if n == 0
        then return()
    else do
        print (repStr "*" n)
        triangle (n-1)
        print (repStr "#" n)


isPrimer :: Int -> Int -> Bool
isPrimer n curr = 
    if curr == 1
        then True
    else if (n `mod` curr ) == 0
        then False
    else
        isPrimer n (curr-1)

isItPrime n = isPrimer n (n-1)

getNextPrime n = 
    if isItPrime (n+1)
        then n+1
    else getNextPrime(n+1)


allPrimeNums n sum nextPrimeNumber = 
    if n == 0
        then sum
    else allPrimeNums (n-1) (sum+nextPrimeNumber) (getNextPrime(nextPrimeNumber))


sumAllPrimes n = allPrimeNums n 0 2

reducerArrayRev array accumulator body = 
    if null array
        then accumulator
    else reducerArrayRev (init array) (body accumulator (last array)) body

revArr array = reducerArrayRev array [] (\acc el -> acc ++ [el])

lengthArr array = reducerArrayRev array 0 (\a e -> a + 1)

readUntil line str = 
    str ++ [([line] ++ "")]

log2 n = myLog 2 n 0


myLog b n sum = 
    if(n == 1)
        then sum
    else do
        myLog b (n `div` 2) (sum+1)


faktoriel' n res = 
    if(n == 1)
        then res
    else faktoriel' (n-1) (res * n)

faktoriel n = faktoriel' n 1

fibonachi' num1 num2 n = 
    if n == 1 || n == 2
        then num2
    else
        fibonachi' num2 (num1 + num2) (n-1)

fibonachi n = fibonachi' 1 1 n

iziTriange n =
    if n <= 0
        then return()
    else do
        print $ repStr "*" n
        iziTriange (n-1)

cutAllNElements array n res = 
    if null array
        then res
    else
        cutAllNElements (drop n array) n (res ++ take (n-1) array )

fibonachiArr' n res = 
    if n <= 0
        then []
    else if n == 1
        then [1]
    else if n == 2
        then res
    else
        fibonachiArr' (n-1) (res ++ [(last res + last (init res))])

fibonachiArr n = fibonachiArr' n [1,1]

faktorielArr' n res index =
    if n<= 0
        then []
    else if index >= n
        then res
    else
        faktorielArr' n (res ++ [faktoriel index]) (index+1)

faktorielArr n = faktorielArr' n [1] 1


main = do

   -- num <- getLine

    --triangle 4

   -- print (sumAllPrimes 50)

    --print (revArr [1,2,3,4,5])


    --izi

    --print (log2 15)

    --print $ faktoriel 10

    --print $ fibonachi 21

    --iziTriange 1

    --

    -- print (lengthArr [123,456])

    -- print (cutAllNElements [1,2,3,4,5,6,7,8,9] 3 [])

    --print $ fibonachiArr (15)

    --print $ faktorielArr 5

    -- let arr = [1, 1, 1, 1, 1, 1,2,3,4,5]

    -- print $ foldl (\a e -> "(" ++ a ++ " + " ++ (show e) ++ ")") ("" ++ (show (head arr))) (tail arr)

    -- print $  reverse (foldr (\e a -> ")" ++ a ++ " + " ++ (show e) ++ "(") ("" ++ (show (last arr))) (init arr))

    -- print $ zipWith (\a1 a2 -> a1 : a2) arr []