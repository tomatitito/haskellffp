module Ch06.InstancesEq where

data TisAnInteger =
       TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'


data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two j1 j2) =
    i1 == j1 && i2 == j2


data StringOrInt = 
    TisAnInt Int
  | TisAString String
  
instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _  = False