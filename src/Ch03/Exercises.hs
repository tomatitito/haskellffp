module Ch03.Exercises where

-- Building functions 1
-- Now write expressions to perform the following transformations, just with the functions you’ve seen in this chapter.
-- You do not need to do anything clever here.

{-
a)
-- Given
     "Curry is awesome"
-- Return
     "Curry is awesome!"
-}

embang s =
  s ++ "!"

{-
b)
-- Given
     "Curry is awesome!"
-- Return
     "y"
-}

index5 s =
  drop 4 $ take 5 s

-- Because of right associativity of `$`, `take 5 s` is evaluated first

{-
c)
-- Given
     "Curry is awesome!"
-- Return
     "awesome!"
-}

last8digits s =
  reverse $ take 8 $ reverse s

thirdLetter :: String -> Char
thirdLetter s =
  s !! 2
