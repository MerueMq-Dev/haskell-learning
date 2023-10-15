inc :: Num a => a -> a
inc v = v + 1

aList :: [String]
aList = ["cat", "dog", "mouse"]

-- class TypeName a where
--     fun 1 :: a -> a
--     fun 2 :: a -> String
-- 	fun 3 :: a -> a -> Bool


class Describable a where
    describe:: a -> String

-- class Eq a => Ord a
--     compare :: a -> a -> Ordering
--     (<) :: a -> a -> Bool
--     (<=) :: a -> a -> Bool
--     (>) :: a -> a -> Bool
--     (>=) :: a -> a -> Bool
--     max :: a -> a -> a
--     min :: a -> a -> a

data Icecream = Chocolate | Vailla deriving (Show, Eq,Ord)
chocolate = Chocolate
vailla = Vailla

bestIcecream = chocolate < vailla

testWord :: Word
testWord = 3

x = succ 2
z = succ '2'

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
    if n == maxBound
        then minBound
        else succ n
