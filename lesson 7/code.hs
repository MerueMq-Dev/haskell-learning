myTake2 count list = 
    if count > 1 && length list /= 0 
    then listHead : (myTake2 newCount listTail)  
    else [listHead]
    where 
        listHead = head list
        listTail = tail list
        newCount = count - 1



findNOD a b = if a `mod` b /= 0 
    then findNOD b remainder
    else b
    where remainder = a `mod` b


sayAmount n = case n of
    1 -> "один"
    2 -> "два"
    n -> "много"

myTail [] = []
myTail (_:xs) = xs

-- моё решение
findNod a b 
    | a `mod` b /= 0 = remainder
    | otherwise = b
    where remainder = a `mod` b

-- решение из книги
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)