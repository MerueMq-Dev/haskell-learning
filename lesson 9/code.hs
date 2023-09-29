import Data.Char (toLower)
myMap _ [] = []
myMap func (x:xs) = (func x) : myMap func xs 

myFilter _ [] = []
myFilter func (x : xs) = r ++ myFilter func xs
    where r = if (func x) then [x] else []

myRemove _ [] = []
myRemove func (x:xs) = if func x 
    then myRemove func xs
    else x: myRemove func xs

myFoldl _ startValue [] = startValue -- изначально этот кейс в моей реализации не обрабатывался
myFoldl operation startValue (x : []) = operation startValue x 
myFoldl operation startValue (x : xs) = operation x $ myFoldl operation startValue xs

-- Какой же я всё таки ахуеный.

myProduct list = myFoldl (*) 1 list


-- Реализация myFoldl из книги

booksFoldl f init [] = init
booksFoldl f init (x:xs) = booksFoldl f newInit xs
    where newInit = f init x

-- Ладно, не такой я уж и ахуеный(
-- Моя реализация foldl не правильно работает совсем
myElem el xs =  0 /= length exp 
    where exp = filter (\x -> x == el) xs 


isPalindrome text = correctWord == reverse correctWord
    where correctWord = map toLower $ filter (\l -> l /= ' ') text


harmonic n = sum (take n seriesValues)
    where seriesPairs = zip (cycle [1.0]) [1.0,2.0 .. ]
          seriesValues = map (\pair -> (fst pair)/(snd pair))
                            seriesPairs