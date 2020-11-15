multiplyArray array n = 
    if null array
        then []
    else
        ((head array) * n) : (multiplyArray (tail array) n)

main = do

    let arr = [1,2,3,4,5]

    print $ multiplyArray arr 5
