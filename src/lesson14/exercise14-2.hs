data FiveSidedDie = One | Two | Three | Four | Five deriving (Show, Eq, Enum)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `div` 5)