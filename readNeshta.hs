removeWhiteSPaces arr spitter = removeAllElsFromArray arr spitter " "


toStringArr array line = 
    array ++ [[line] ++ ""]

toString' array line = 
    array ++ [line]

toString line = toString' "" line

removeAllElsFromArray arr spitter element =

        if spitter == element
            then arr
        else
            arr ++ [spitter]


findNextIndex :: String -> String -> Int -> Int
findNextIndex array splitter currIndex = 
    if null array
        then -1
    else if (take (length splitter) array) == splitter
        then currIndex+(length splitter)
    else
        findNextIndex (tail array) splitter (currIndex+1)


split :: String -> String -> String -> String
split array splitter resArray = 

    if null array
        then resArray
    else 
        split (drop (findNextIndex array splitter 0) array) splitter (resArray ++ take (findNextIndex array splitter 0 - length splitter) array)
        

main = do 

    let str = "1.a.do().a.8923.op"

    print ( findNextIndex str ".a." 0)

    let newStr = split str ".a." []

    print newStr

-- line <- getLine

-- print ((foldl toStringArr [] line) == ["e","n","d"])

-- print $ foldl removeWhiteSPaces [] (foldl toStringArr [] line)