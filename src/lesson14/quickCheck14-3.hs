data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

instance Show SixSidedDie where
  show S1 = "Ⅰ"
  show S2 = "Ⅱ"
  show S3 = "Ⅲ"
  show S4 = "Ⅳ"
  show S5 = "Ⅴ"
  show S6 = "Ⅵ"

instance Eq SixSidedDie where
  (==) S6 S6 = True
  (==) S5 S5 = True
  (==) S4 S4 = True
  (==) S3 S3 = True
  (==) S2 S2 = True
  (==) S1 S1 = True
  (==) _ _ = False

instance Ord SixSidedDie where
  compare S6 S6 = EQ
  compare S6 _  = GT
  compare _  S6 = LT
  compare S5 S5 = EQ
  compare S5 _  = GT
  compare _  S5 = LT
  compare S4 S4 = EQ
  compare S4 _  = GT
  compare _  S4 = LT