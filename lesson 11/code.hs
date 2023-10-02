
-- myAverage:: Int -> Double
-- myAverage aList = (fromIntegral (sum aList)) / length aList

half:: Int -> Double
half n = fromIntegral n / 2

divHalf:: Integer -> Integer
divHalf n = n `div` 2

printDouble:: Int -> String
printDouble n = show $ n * 2

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter func (x : xs) = r ++ myFilter func xs
  where
    r = if (func x) then [x] else []


-- myFoldl:: (a -> b -> a) -> a -> [b] -> a 