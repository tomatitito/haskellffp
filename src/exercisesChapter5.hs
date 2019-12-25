{-# LANGUAGE NoMonomorphismRestriction #-}

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

-- determine the type

{-
1. All function applications return a value. Determine the value
returned by these function applications and the type of that
value.
-}

a = (* 9 ) 6

b = head [(0, "doge"), (1, "kitteh")]

c = head [(0 :: Integer ,"doge"),(1,"kitteh")]

d = if False then True else False

e = length [1, 2, 3, 4, 5]

f = (length [1, 2, 3, 4]) > (length "TACOCAT")


-- 2. Given
ans2 = w
  where
    x = 5
    y = x + 5
    w = y * 10
  
-- What is the type of w?

-- 3. Given
ans3 = z
  where
    x = 5
    y = x + 5
    z y = y * 10

-- What is the type of z?

-- 4. Given
ans4 = f
  where
    x = 5
    y = x + 5
    f = 4 / y

-- What is the type of f4?

-- 5. Given
ans5 = f
  where 
    x = "Julie" 
    y = " <3 "
    z = "Haskell"
    f = x ++ y ++ z

-- What is the type of f?