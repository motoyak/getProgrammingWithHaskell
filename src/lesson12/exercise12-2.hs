data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName
type FirstName = String
type LastName = String
type MiddleName = String

data Sex = Male | Female

data BloodType = BloodType ABOType RhType
data ABOType = A | B | AB | O
data RhType = Pos | Neg
type Age = Int
type Height = Int

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Patient = Patient { name :: Name
                        ,sex :: Sex
                        ,age :: Int
                        ,height :: Int
                        ,weight :: Int
                        ,bloodType :: BloodType }
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

patientSummary :: Patient -> String
patientSummary p =    "**************\n"
                   ++ "Patient Name: " ++ showName (name p) ++ "\n"
                   ++ "Sex: " ++ showSex (sex p) ++ "\n"
                   ++ "Age: " ++ show (age p) ++ "\n"
                   ++ "Height: " ++ show (height p) ++ " in." ++ "\n"
                   ++ "Weight: " ++ show (weight p) ++ " lbs." ++ "\n"
                   ++ "Blood Type: " ++ showBloodType (bloodType p) ++ "\n"
                   
jackieSmith :: Patient
jackieSmith = Patient {name = Name "Jackie" "Smith"
                       , age = 43
                       , sex = Female
                       , height = 62
                       , weight = 115
                       , bloodType = BloodType O Neg }