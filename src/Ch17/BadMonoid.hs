module Ch17.BadMonoid where

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
  
-- with this implementation it's not a bad monoid anymore
instance Monoid Bull where
  mempty = Fools
  mappend Fools Fools = Fools
  mappend Fools Twoo = Twoo
  mappend Twoo _ = Twoo

-- EqProp is from the checkers library
instance EqProp Bull where
  (=-=) = eq

main:: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch (semigroup (Fools, 5 :: Int))
