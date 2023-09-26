ifEven :: Integral t => (t -> t) -> t -> t
ifEven myFunction x  =
    if even x
        then myFunction x
        else x


genIfEven :: Integral t => (t -> t) -> t -> t
genIfEven f = (\x -> ifEven f x)

-- ifEvenInc = genIfEven (\x -> x + 1)

genIfEvenX :: Integral t => t -> (t -> t) -> t
genIfEvenX arg = (\function -> ifEven function arg)

testFunc = genIfEvenX 10

getRequestUrl host apiKey resource id = 
    host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id -> 
    getRequestUrl host apiKey resource id)

genApiKeyRequestBuilder hostBulder apiKey = (\resource id -> hostBulder apiKey resource id)

genResourceRequestBuilder apiKeyBulder resource = (\id -> apiKeyBulder resource id)

exampleHostBuilder = genHostRequestBuilder "https://example.com"
exampleUrlBuilder = genApiKeyRequestBuilder exampleHostBuilder "testKey322"
exampleApiKeyBuilder = genApiKeyRequestBuilder exampleHostBuilder "testKey322"

genApiRequestBuilder :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
genApiRequestBuilder hostBuilder apiKey resource =
    (\id -> hostBuilder apiKey resource id)

secondExapleApiRequstBuilder hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

urlBuilder = secondExapleApiRequstBuilder exampleHostBuilder "Solo322" "DodgeList"

-- Частичное применение!!!!

secondUrlBuilder :: [Char] -> [Char]
secondUrlBuilder = getRequestUrl "https://exmaple.com" "1337hAsk3ll" "book"

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)


subtract2 :: Integer -> Integer

subtract2 = flip (-) 2
-- Задания в конце урока
inc n = n + 1
double n = n * 2  
square n = n ^ 2

ifEvenInc x = ifEven inc x 
ifEvenDouble x = ifEven double x 
ifEvenSquare x = ifEven square x 

binaryPartialApplication f a = (\x -> f a x) 

binaryTest = binaryPartialApplication (+) 2


