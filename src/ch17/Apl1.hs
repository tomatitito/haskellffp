module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Semigroup a, Monoid a) => Semigroup (ZipList a) where
  (<>) = mappend 

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend
 
-- the following instances are already implemented 
{-
instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary
  
instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary
-}
  
instance Eq a => EqProp (ZipList a) where
  (=-=) = eq
  
main :: IO ()
main = do
  let zl = ZipList [1 :: Sum Int]
  quickBatch (monoid zl)


