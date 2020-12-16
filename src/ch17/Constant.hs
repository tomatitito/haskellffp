module Constant where

newtype Constant a b =
  Constant { getConstant :: a } -- b is ghost, it does only appear in the constructor
  deriving (Eq, Ord, Show)
  
instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
  
instance Monoid a => Applicative (Constant a) where
  pure _ = (Constant mempty) 
  Constant x <*> Constant y = Constant $ mappend x y

