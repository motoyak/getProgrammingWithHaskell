type PatientName = (String,String)

patientInfo :: PatientName -> Int -> Int -> String
patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
                                     where name = lname ++ ", " ++ fname
                                           ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"