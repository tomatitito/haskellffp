module BadMonoid where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [(1, return Fools)
              ,(1, return Twoo)]

instance Semigroup Bull where
  (<>) = mappend
  
instance Monoid Bull where
  mempty = Fools
  mappend Fools Fools = Fools
  mappend Fools Twoo = Twoo
  mappend Twoo _ = Twoo

-- EqProp is from the checkers library
instance EqProp Bull where
  (=-=) = eq

main:: IO ()
main = quickBatch (monoid Twoo)