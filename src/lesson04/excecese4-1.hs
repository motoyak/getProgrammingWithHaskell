import Data.List

compareLastNames :: (String,String) -> (String,String) -> Ordering
compareLastNames name1 name2 = if result == EQ
                               then compare firstName1 firstName2
                               else result
                               where lastName1 = snd name1
                                     lastName2 = snd name2
                                     firstName1 = fst name1
                                     firstName2 = fst name2
                                     result = compare lastName1 lastName2
                                     
names = [("Bernard","Sumner"),("Ian", "Curtis"),("Peter", "Hook"),("Stephen","Morris"),("Zotohiro","Hook"),("Zotohiro","Hook")]