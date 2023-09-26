import Data.List

ifEven :: Integral t => (t -> t) -> t -> t
ifEven myFunction x =
    if even x
        then myFunction x
        else x

b = ifEven (\x -> x ^ 3) 2

names = [("Иэн", "Кертис"), ("Бернард", "Самнер"), ("Питер", "Хук"), ("Стивен", "Моррис")]


compareLastNames name1 name2 = compare lastName1 lastName2
    where firstName1 = fst name1
          lastName1 = snd name1
          fistName2 = fst name2 
          lastName2 = snd name2

testFunc name1 name2 = sortBy compareLastNames [(" ",name1), (" ",name2)] 

sfOffice name = if lastName < "Л" 
                    then nameText ++ " - А/я 1234, Сан-Франциско, штат Калифорния, 94111"
                    else nameText ++ " - А/я 1010, Сан-Франциско, штат Калифорния, 94109"
                    where lastName = snd name
                          nameText = (fst name) ++ " " ++ lastName


nyOffice name = nameText ++ ": А/я 789, Нью-Йорк, штат Нью-Йорк, 10013"
            where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - А/я 456, Рино, штат Невада, 89523"
            where nameText = snd name

washingtonOffice name = "Dear " ++ fullName ++ " - А/я 456, Рино, штат Невада, 89523"
    where fullName = fst name ++ " " ++ snd name


getLocationFunction location = 
    case location of
        "ny" -> nyOffice
        "sf" -> sfOffice
        "reno" -> renoOffice
        "WDC"-> washingtonOffice
        _ -> (\name -> 
             (fst name) ++ " " ++ (snd name))