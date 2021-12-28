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

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

canDonateTo' :: Patient -> Patient -> Bool
canDonateTo' p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

someFunc :: IO ()
someFunc = putStrLn "Hello"