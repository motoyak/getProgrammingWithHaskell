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


jackieSmith :: Patient
jackieSmith = Patient {name = Name "Jackie" "Smith"
                       , age = 43
                       , sex = Female
                       , height = 62
                       , weight = 115
                       , bloodType = BloodType O Neg }