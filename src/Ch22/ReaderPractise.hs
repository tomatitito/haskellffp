module Ch22.ReaderPractise where

import Control.Applicative
import Data.Maybe
import Prelude hiding (lookup, uncurry)

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup key pairs
  | null lookedUp = Nothing
  | otherwise = Just $ snd (head lookedUp)
  where lookedUp = filter (\(x, y) -> x == key) pairs

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
--x1 = liftA2 (,) xs ys
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

summed :: Num c => (c, c) -> c
summed pair = uncurry (+) pair

bolt :: Integer -> Bool
bolt n = n>3 && n<8

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]
-- f ~ (->) a and t ~ []
-- We have a Reader for the Applicative (functions) and a traversable for the list.

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
{-
  print $ sequenceA [Just 3, Just 2, Just 1]

  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]

  print $ summed <$> x1
  print $ summed <$> ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
-}
  print $ sequenceA [(>3), (<8), even] 7
  -- 1. fold the boolean conjunction operator over the list of results of sequA (applied to some value).
  print $ foldr (&&) True (sequA 7)
  -- 2. apply sequA to s'; you’ll need fromMaybe.
  print $ fromMaybe [False] $ (Just $ sequA) <*> s'
  -- 3. apply bolt to ys; you’ll need fromMaybe. 
  print $ fromMaybe False $ (Just bolt) <*> ys 
