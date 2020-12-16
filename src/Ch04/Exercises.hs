module Ch04.Exercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l =
  (reverse l) == l

myAbs :: Integer -> Integer
myAbs x =
  if x >= 0 then x else (-x)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) =
  ((b, d), (a, c))

-- Correcting syntax
{-
1. Here, we want a function that adds 1 to the length of a string argument and returns that result.

    x = (+)
    F xs = w 'x' 1
         where w = length xs
-}

x = (+)
f2 xs = w `x` 1
  where w = length xs


{-
3. When fixed, this function will return 1 from the value (1, 2).

   f (a b) = A
-}

f3 (a, b) = a

