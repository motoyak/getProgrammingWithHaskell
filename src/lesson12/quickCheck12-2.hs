data Patient = Patient Name Sex Int Int Int BloodType

janeSmith :: Patient
janeSmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith")
                    Female 22 60 135 (BloodType B Pos)