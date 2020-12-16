module Ch21.Exercises where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- write a Traversable instance for Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Foldable Identity where
  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr f initial (Identity a) = f a initial
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
--  foldMap = undefined  -- minimally implement either one
--  foldr = undefined
--  foldl = undefined
--  foldMap = undefined

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a =  Identity a
  Identity f <*> Identity a = Identity (f a)

instance Traversable Identity where
  -- (a -> f b) -> t a -> f (t b)
  traverse f (Identity x) = Identity <$> f x
 
identityGen :: Arbitrary a => Gen (Identity a) 
identityGen = do
  x <- arbitrary
  return $ Identity x

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen
  
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

type TI = Identity

run :: IO ()
run = do -- (Identity 42)
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (functor $ Identity (2 :: Int, 2 :: Int, 2 :: Int))
  quickBatch (applicative [("b", "w", 1 :: Int)])
  quickBatch (traversable trigger)

