abs5 a = do
    (abs a)+5

abs5' a
    | a > 0 = a+5
    | a < 0 = (-a) + 5
    | otherwise = 5


filterMe array filterBody = filter filterBody array

squarRootMe x = sqrt x

squareMe x = x*x

compose f1 f2 x = f1 (f2 x)

absoluteVal x = compose squarRootMe squareMe x
    

add :: [a] -> a -> [a]
add array n = do array ++ [n]

print5 = do
    print 5

printa = do
    print "ako"

simpFun a = do
    if a == 5
        then print5
    else if a == 6
        then print "6"
    else printa

max1 a b
    | a > b = "The gr num is "++ (show a)
    | a < b = "The gr num is "++ (show b)
    | a == b = "the num are eq"
    | otherwise = "Nonething"

switchCase a = case a of
    1 -> "One"
    2 -> "Two"
    3 -> "Three"
    4 -> "Four"
    5 -> "Five"
    _ -> "Invalid!"

main = do
    let a =5
    let b = 10
    let list = [1,2,3]
    let addedArr = add list 5

    putStr "The addition of a and b: "
    print(a + b)

    putStr "The subst of a and b: "
    print(a-b)

    print[1..10]

    print(max1 5 6)

    print (addedArr)

    simpFun 6

    print(switchCase 8)

    print(abs5 (-5));
    print(abs5'(-5));

    print(round(absoluteVal (-5)))

    --line <- getLine
    --putStrLn ("Hello" ++ line)

   -- file <- readFile "test1.hs"

    --putStrLn file