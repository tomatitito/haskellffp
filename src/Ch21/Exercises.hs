module Ch21.Exercises where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  x <- arbitrary
  return $ Identity x

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

constantGen :: Arbitrary a => Gen (Constant a b)
constantGen = do Constant <$> arbitrary

triggerFunctor :: (Int, Int, Int)
triggerFunctor = undefined

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Foldable Identity where
  -- minimally implement either one
  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr f initial (Identity a) = f a initial

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a =  Identity a
  Identity f <*> Identity a = Identity (f a)

instance Traversable Identity where
  -- (a -> f b) -> t a -> f (t b)
  traverse f (Identity x) = Identity <$> f x
 
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

newtype Constant  a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = constantGen

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  -- (b -> m) -> t b -> m
  -- (b -> m) -> C a b -> m
  foldMap _ c = mempty
  
run :: IO ()
run = hspec $ do
  describe "Testing laws for instances" $ do
    it "Identity" $ do
      let trigger = undefined :: Identity (Int, Int, [Int])
      quickBatch (functor $ Identity triggerFunctor)
      quickBatch (applicative $ Identity ("b", "w", 1 :: Int))
      quickBatch (foldable $ Identity (42 :: Integer, 42 :: Integer, "42", 42 :: Integer, 42 :: Integer))
      quickBatch (traversable trigger)
    it "Constant" $ do
      let cFunctor :: Constant Int (Int, Int, Int)
          cFunctor = Constant 2 -- triggerFunctor
          cFoldable :: Constant Int (Int, Int, String, Int, Int)
          cFoldable = Constant 42
      quickBatch (functor cFunctor)
      quickBatch (foldable cFoldable)

