myDrop _ []  = []
myDrop 0 list = list
myDrop count list = myDrop newCount $ tail list  
    where newCount = count - 1


myLength [] = 0
myLength (_:xs) = 1 + myLength xs  

-- мой код с прошлого урока, начало:
myTake2 count list = 
    if count > 1 && length list /= 0 
    then listHead : (myTake2 newCount listTail)  
    else [listHead]
    where 
        listHead = head list
        listTail = tail list
        newCount = count - 1
-- конец

myTake3 0 _ = []
myTake3 _ [] = []
myTake3 1 (x:xs) = [x]
myTake3 count (x:xs) = x : myTake3 newCount xs 
    where newCount = count - 1


myCycle (x:xs) = x : myCycle(xs++[x])  


-- collatz 1 = 1
-- collatz n = 
--     if even n 
--     then 1 + collatz (n `div` 2)
--     else 1 + collatz (n * 2 + 1)
    -- where 
    --     nEven = n `div` 2
    --     nOdd = n * 2 + 1


collatz 1 = 1
collatz n =
    if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n*3 + 1)


-- задачи
myReverse [] = []
myReverse (x : []) = [x]
myReverse (x : xs) = myReverse xs ++ [x]

fastFib _ _ 0 = 0 
fastFib _ _ 1 = 1 
fastFib _ _ 2 = 1 
fastFib n1 n2 3 = n1 + n2 
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)

-- Прошёл 8 урок, не до конца обработал все кейсы. Надо будет ещё раз перечитать 2 последних урока!