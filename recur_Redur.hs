main :: IO()

repeatString str n = 
        if n == 0
            then ""
        else str ++ (repeatString str (n-1))

forEach2 array action index = do
    if index >= length(array)
        then return()
    else do
        action (array!!index)
        forEach2 array action (index+1)

forEach array action = do
    forEach2 array action 0

forLoop i condition step body =
    if not(condition i)
        then return()
    else do
        body i
        forLoop (i+step) condition step body



int a = 2

main = do

    --print(repeatString "M" 4)

    --forEach [1,2,3,4,5,6,7] (\x -> print(x))

    forLoop 1 (\x -> x <= 10) 1 (\x -> print(x))
        