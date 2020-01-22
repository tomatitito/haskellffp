{-# LANGUAGE NoMonomorphismRestriction #-}

module Chapter5 where

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

-- Determine the type

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

-- Does it compile? (dic)
-- Everything commented out doesn't.

{- 
bignum = (^) 5 $ 10
wahoo = bignum $ 10
-}

dic2 = z
  where
    x = print
    y = print "whoohoo!"
    z = x "hello world"
    
{-
dic3 = d
  where
    a = (+)
    b = 5
    c = b 10
    d = c 200
-}

{- Variable not in scope: cc
aa = 12 + bb 
bb = 10000 * cc
-}

-- Type variable or specific type constructor?

{-
1. You will be shown a type declaration, and you should categorize
each type. The choices are a fully polymorphic type variable,
constrained polymorphic type variable, or concrete type constructor.

f :: Num a => a -> b -> Int -> Int
  --         [0]  [1]   [2]    [3]
  Here, the answer would be: constrained polymorphic (Num) ([0]),
  fully polymorphic ([1]), and concrete ([2] and [3]).
-}

{-
2.
f :: zed -> Zed -> Blah
     [0]    [1]    [2]
  [0] is a fully polymorphic type, [1] and [2] are concrete types.
-}

{-
3. 
f :: Enum b => a -> b -> C
     [0]       [1]  [2]  [3]
[0] is a constrained polymorphic type, as is [2]. [1] is fully polymorphic,
[3] is a concrete type.
-}

{-
4. 
f :: f -> g -> C
     [0]  [1]  [2]
[0] is a function so it should be concrete, [1] is fully polymorphic, [2] is concrete.
-}


-- Write a type signature
{-
For the following expressions, please add a type signature. You should
be able to rely on GHCi type inference to check your work, although
you might not have precisely the same answer as GHCi gives (due to
polymorphism, etc).
-}

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = 
  if (x > y) then True else False

functionS :: (a, b) -> b 
functionS (x, y) = y


-- Given a type, write a function 

{-
1. There is only one function definition that typechecks and doesn’t
go into an infinite loop when you run it.
-}
i :: a -> a
i a = a

-- 2. There is only one version that works.
given_the_type_c :: a -> b -> a
given_the_type_c x y = x

{-
3. Given alpha equivalence are c'' and c (see above) the same
thing?
-}
c'' :: b -> a -> b
c'' = given_the_type_c

-- 4. Only one version that works.
c' :: a -> b -> b
c' x y = y

{-
5. There are multiple possibilities, at least two of which you’ve
seen in previous chapters.
-}
r :: [a] -> [a]
r l = l

{-
6. Only one version that will typecheck.
-}
co :: (b -> c) -> (a -> b) -> a -> c
co bToc aTob a= 
  (bToc (aTob a))
 
-- 7. Only one version will typecheck. 
given_the_type_7 :: (a -> c) -> a -> a
given_the_type_7 f a = a 

-- 8. One version will typecheck.
a' :: (a -> b) -> a -> b
a' aTob a = aTob a


-- Type-kwon-do

{-
1.

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g $ f x 
-}

{-
2.

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x
-}


{-
3.

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = ((xz x), (yz y))
-}

{-
4. 

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xy ywz x = fst (ywz (xy x))

Function application is left associative. Therefore this is equivalent to the above:
    munge xy ywz x = fst (ywz $ xy x)
while this is not
    munge xy ywz x = fst (ywz xy x)
-}
munge xy ywz x = fst ywz (xy x)
