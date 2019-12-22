module ExercisesChapter5 where

-- Exercises Parametricity

{-
1. Given the type a -> a, which is the type for id, attempt to make
a function that terminates successfully that does something
other than returning the same value. This is impossible, but you
should try it anyway.
-}

--impossible :: a -> a
--impossible a = a + 1
-- is actually possible to type the above signature into the repl. In that case the type signature
-- is changed to Num a => a -> a. However, compiling fails.


{-
2. We can get a more comfortable appreciation of parametricity
by looking at a -> a -> a. This hypothetical function a -> a ->
a has two –and only two– implementations. Write both possible
versions of a -> a -> a. After doing so, try to violate the
constraints of parametrically polymorphic values we outlined
above.
-}

hypothetical1 :: a -> a -> a
hypothetical1 a b = a

hypothetical2 :: a -> a -> a
hypothetical2 a b = b

-- will not compile:
--hypothetical3 :: a -> a -> a
--hypothetical3 a b = a + b


{-
3. Implement a -> b -> b. How many implementations can it
have? Does the behavior change when the types of a and b
change?
-}

hypothetical4 :: a -> b -> b
hypothetical4 a b = b

-- this should be the only possible implementation
