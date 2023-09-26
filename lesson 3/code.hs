

isEven :: Int -> Int
isEven number = if even number then number - 2 else 3 * number + 1

inc :: Int -> Int
inc number = number + 1

double:: Int -> Int
double number = number * 2

-- square:: Int -> Float
square number = number ** 2 

doubleDouble number = (\double -> double  $ double number)

value = 5

overwrite x = (\x -> 
                (\x ->
                    (\x -> x) 4) 3) 2


counter x = (\x -> (\x -> x + 1) x + 1) x