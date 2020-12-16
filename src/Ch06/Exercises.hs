module Ch06.Exercises where

import Data.List

-- Does it typecheck?

-- 2.
-- This won't typecheck because Mood doesn't derive Eq:

{-
data Mood = Blah
          | Woot deriving Show

settleDown x = if x == Woot
                 then Blah
                 else x
-}

-- Fix:

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                 then Blah
                 else x

-- 3.
-- a) What values are acceptable inputs to this function? Only values of type Mood.
-- b) What will happen if you try to run settleDown with 9? The compiler will complain because there is no Instance for
--    (Num Mood). So it seems, even though Mood is not a type class (is it?), since the function expects a Mood,
--    the compiler checks if the given argument has an instance of Mood.
-- c) What will happen if you try to run Blah > Woot? the compiler will complain, because there is no instance for
--    (Ord Mood).

-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- This will typecheck. However, trying to print s1 will result in an error because of the type of s1, which is
-- Object -> Sentence. It's a function, and functions don't have instances for Show. Apparently the type constructor is
-- a function, which can also be curried.

-- Given a datatype declaration, what can we do?

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- The following does not typecheck:
--phew = Papu "chases" True
-- Fix:
phew = Papu (Rocks "chases") (Yeah True)

truth =   Papu (Rocks "chomxkydoz")
               (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- The following won't compile, because there is no instance for (Ord Papu)
{-
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
-}

-- Match the types

i :: Num a => a
--i :: a -- won't compile
i = 1

f :: Float
--f :: Num a => a -- won't compile
f = 1.0

ff:: Float
--ff: Fractional a => a -- will compile as well

ff = 1.0

freud :: a -> a
--freud Ord a => a -> a -- will compile
freud x = x

--freud' :: a -> a
freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
--sigmund :: a -> a -- won't compile
sigmund x = myX


jung :: Ord a => [a] -> a
--jung :: [Int] -> Int -- will compile
jung xs = head (sort xs)

young :: [Char] -> Char
--young :: Ord a => [a] -> a -- will compile
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a -- won't compile
signifier xs = head (mySort xs)

-- Type-Kwon-Do

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aTob a b = b == aTob a

