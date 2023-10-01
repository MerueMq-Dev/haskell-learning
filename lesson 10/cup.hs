cupConstructor ml = \message -> message ml
aCup = cupConstructor 500

getMl aCup = aCup (\ml -> ml) 

dobuleSuzeCup oldCup = cupConstructor (ml * 2) 
    where ml = getMl oldCup

drink aCup mlDrank = if mlDrank > ml 
    then cupConstructor 0
    else cupConstructor (ml - mlDrank)
    where ml = getMl aCup

afterManySips = foldl drink aCup [30, 40,50, 20] 

