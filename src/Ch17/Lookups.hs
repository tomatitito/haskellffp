module Ch17.Lookups where

import Control.Applicative (liftA, liftA2)
import Data.List (elemIndex)

-- 1.
added :: Maybe Integer
added =
  pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])


-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z


-- 3.
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5, 6]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5, 6]

max' :: Maybe Int -> Maybe Int -> Maybe Int
max'  a b =  pure max <*> a <*> b

maxed :: Maybe Int
maxed = max' x y'


-- 4.
xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = pure sum <*> (pure (,) <*> x'' <*> y'')
-- same as
-- pure sum <*> ((,) <$> x'' <*> y'')