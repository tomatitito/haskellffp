module MonadInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg
  deriving (Show)

instance Functor Nope where
  -- fmap :: (a -> f b) -> f a -> f b
  fmap f NopeDotJpg = NopeDotJpg
  
instance Applicative Nope where
  pure = \_ -> NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg
   
instance Monad Nope where
  return = pure
  -- m a -> (a -> m b) -> m b
  NopeDotJpg >>= f = NopeDotJpg
  
  
main = do
--  let xs = ["b", "w", 1] 
  quickBatch $ functor [(1 :: Int, 1 :: Double, "a")]
  quickBatch $ applicative [(1 :: Int, 1 :: Double, "a")]

