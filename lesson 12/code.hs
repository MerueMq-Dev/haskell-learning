type FirstName = String
type LastName = String
type MiddleName = String
type Age = Int
type Height = Int
type Weight = Int
type PatientName = (String, String) 

-- fistName :: PatientName -> FirstName
-- fistName patient = fst patient 

-- lastName :: PatientName -> LastName
-- lastName patient = snd patient


-- patientInfo :: PatientName -> Age -> Height -> String
-- patientInfo patientName age height = name ++ " " ++ ageHeight
--     where name = fistName patientName ++ ", " ++ lastName patientName
--           ageHeight = "(Age: " ++ show age ++ "; height: " ++ show height ++ "sm)"

data Sex = Male | Female

sexInitial:: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'


data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType {aboType :: ABOType, rhType :: RhType }

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT:: BloodType
patient2BT = BloodType O Neg

patient3BT:: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

-- data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

-- canDonateTo :: BloodType -> BloodType -> Bool
-- canDonateTo (BloodType O _) _ = True
-- canDonateTo _ (BloodType AB _) = True
-- canDonateTo (BloodType A _) (BloodType A _) = True
-- canDonateTo (BloodType B _) (BloodType B _) = True
-- canDonateTo _ _ = False



showName:: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l 
data Patient = Patient {name :: Name, sex :: Sex, age :: Int, heigth :: Int, weigth :: Int, bloodType :: BloodType}
-- Jane Elizabeth Smith
janeBloodType = BloodType O Pos

elizabethName = NameWithMiddle "Jane" "Elizabeth" "Smith"

janePatient :: Patient
janePatient = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 26 159 50 (BloodType A Pos)

jackiePatient :: Patient
jackiePatient = Patient { name = Name "Jakie" "Smith", age = 43, sex = Female, weigth = 56, heigth = 164, bloodType = BloodType B Neg }

data Name = Name {firstName :: String, lastName :: String} | NameWithMiddle {firstName :: String, middleName :: String,  lastName :: String}


-- Моя версия решения задачи :(
-- canDonateTo :: Patient -> Patient -> Bool
-- canDonateTo (Patient _ _ _ _ _ (BloodType O _)) _ = True
-- canDonateTo  _ (Patient _ _ _ _ _ (BloodType AB _))  = True
-- canDonateTo (Patient _ _ _ _ _ (BloodType A _)) (Patient _ _ _ _ _ (BloodType A _)) = True
-- canDonateTo (Patient _ _ _ _ _ (BloodType B _)) (Patient _ _ _ _ _ (BloodType B _)) = True
-- canDonateTo _ _ = False

-- Начало решения из книги:
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

donorFor :: Patient -> Patient -> Bool
donorFor p1 p2 = canDonateTo (bloodType p1) (bloodType p2)
-- Конец решения


showTypeOfBlood :: BloodType -> String
showTypeOfBlood bloodType = showABO (aboType bloodType) ++ showRh (rhType bloodType)

showSex Male = "Male"
showSex Female = "Female`"

-- Мое решение
patientSummary :: Patient -> String
patientSummary patient = "************** \n" ++ "Patient name: " ++ patientName ++ "\n" ++ 
                        "Sex: " ++ patientSex ++ "\n" ++
                        "Age: " ++ patientAge ++ "\n" ++
                        "Height :" ++ patientHeight ++ "\n" ++
                        "Weigth: " ++ patientWeigth ++ "\n" ++ 
                        "Type of blood: " ++ patientTypeOfBlod ++ "\n" ++
                        "**************"
    where patientName = showName (name patient)
          patientSex = showSex (sex patient)
          patientAge = show (age patient)
          patientHeight = show (heigth patient) ++ " sm"
          patientWeigth = show (weigth patient) ++ " kg"
          patientTypeOfBlod = showTypeOfBlood (bloodType patient)

-- Решение из книги
-- patientSummary :: Patient -> String
-- patientSummary patient =
--     "**************\n" ++
--     "Пол: " ++ showSex (sex patient) ++ "\n" ++
--     "Возраст: " ++ show (age patient) ++ "\n" ++
--     "Рост: " ++ show (height patient) ++ "см\n" ++
--     "Вес: " ++ show (weight patient) ++ "кг.\n" ++
--     "Тип крови: " ++ showBloodType (bloodType patient)
--     ++ "\n**************\n" 