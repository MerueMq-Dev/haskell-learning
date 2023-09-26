back = reverse [1..]

assignToGroups n aList = zip groups aList
    where groups = cycle [1..n]



-- Задачи

myRepeat x = cycle [x] 

subseq:: Int -> Int -> [a] -> [a]
subseq start end list = take skipValue $ drop start list
    where skipValue = end - start

-- inFirstHalf::Eq a => [a] -> a -> Bool
inFirstHalf list value =  elem value $ take halfLength list
    where halfLength = length list `div` 2 

-- inFirstHalf val myList = val `elem` firstHalf
-- 	where midpoint = (length myList) `div` 2
-- 	      firstHalf = take midpoint myList