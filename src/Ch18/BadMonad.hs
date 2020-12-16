module Ch18.BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)
  
instance Functor CountMe where
  fmap f (CountMe i a) =
    -- CountMe (i + 1) (f a) fails the functor laws
    CountMe i (f a)

instance Applicative CountMe where
  -- laws:
  -- identity:
  --   pure id <*> v = v
  -- composition:
  --   pure (.) <*> u <*> v <*> w =
  --     u <*> (v <*> w)
  -- homomorphism:
  --   pure f <*> pure x = pure (f x)
  -- interchange:
  --   u <*> pure x = pure ($ x) <*> u
  pure = CountMe 0
  -- CountMe n f <*> CountMe _ a = CountMe (n + 1) (f a) fails the applicative laws
  CountMe n f <*> CountMe n' a =
    CountMe (n + n') (f a)

test =
  pure (.) <*> CountMe 1 (++ "a") <*> CountMe 42 (++ "a") <*> CountMe 0 "a"

test2 =
  CountMe 1 (++ "a") <*> (CountMe 42 (++ "a") <*> CountMe 0 "a")

instance Monad CountMe where
  -- laws:
  -- identity laws:
  --   m >>= return = m
  --   return x >>= f = fx
  -- associativity:
  --   (m >>= f) >>= g = m >>= (\x -> f x >>= g)
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

-- Note: For this to abide all laws, CountMe must be a Monoid. The neutral element must be CountMe 0, and appending to
-- CountMe's must add the integers. In that case, it's not a bad Monad anymore.

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary =
    CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger