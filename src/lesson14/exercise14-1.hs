data Number = One | Two | Three deriving Enum

instance Eq Number where
  (==) n1 n2 = fromEnum n1 == fromEnum n2

instance Ord Number where
  compare n1 n2 = compare (fromEnum n1) (fromEnum n2)