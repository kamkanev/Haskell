
repStr str n = do
    if n == 0
        then ""
    else 
        str ++ ( repStr str (n-1))


forloop i condition step body = 
    if not(condition i)
        then return()
    else do
        body i
        forloop (i + step) condition step body


sumNumbers sum currNum maxNum = 
    if currNum > maxNum
        then sum
    else sumNumbers (sum+currNum) (currNum+1) maxNum

sumfirst10natNums = sumNumbers 0 1 10


reduserArray array accumulator body = 
    if length (array) == 0
        then accumulator
    else 
         reduserArray (tail array) (body accumulator (head array)) body

sumArrayNums array = reduserArray array 0 (\accumulator element -> accumulator + element)

mergeNumsString array = reduserArray array "" (\accumulator elemet -> accumulator ++ show (elemet))

createNewArr array = reduserArray array [] (\acc el -> acc ++ [el+1])

dupEvryElement array = reduserArray array [] (\a e -> a ++ [e] ++ [e])
main = do

print ( repStr "|-|" 3)

print sumfirst10natNums

print (sumArrayNums [1,2,3,4,5,6,7,8,9,10])

print (mergeNumsString [1,2,3,4,5,6,7,8,9,10])

print (createNewArr [1,2,3,4,5,6,7,8,9,10])

print (dupEvryElement [1,2,3,4,5])

print (foldl (\acc el -> acc ++ (show el)) "" [1,2,3,4,5])

-- print (foldr (\acc el -> acc ++ (show el)) "" ["1","2","3","4","5"])


-- forloop 1 (\x -> x <= 10) 1 (\x -> print x)