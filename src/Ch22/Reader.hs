{-# LANGUAGE InstanceSigs #-}

module Ch22.Reader where

import Test.Hspec
--import Control.Monad.Reader
import Control.Applicative (liftA2)
import Control.Monad (return)
import Data.Char


newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
--  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader (fmap f ra)
  -- or:               Reader (f . ra)
--  fmap f r = Reader r ( f (runReader r))

boop = (*2)
doop = (+10)


bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

tupled :: [Char] -> ([Char], [Char])
tupled xs = do
  let capped = cap xs
  let reversed = rev xs
  (capped, reversed)

tupled' :: [Char] -> ([Char], [Char])
tupled' xs =
  return (cap xs) >>= \x -> (x, rev xs)

--instance Applicative (Reader r) where
--  pure :: a -> Reader r a
--  pure a = Reader $ \_ -> a
--
--  (<*>) :: Reader r (a -> b)
--        -> Reader r a
--        -> Reader r b
--  (Reader rab) <*> (Reader ra) =
--    Reader $ \r -> rab ra

-- Exercise: Ask
ask :: Reader a a
ask = Reader id

-- Exercise: Reading comprehension
-- 1.
myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 fun x y =
  fun <$> x <*> y

-- 2.
asks :: (r -> a) -> Reader r a
asks f = Reader f

-- 3.
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader $ const x
  --       f (a -> b)        -> f a        -> f b
  -- f ~ (r ->)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  -- rab ~ r -> a -> b
  Reader rab <*> Reader ra = Reader (rab <*> ra)

-- Exercise: Reader Monad
-- 1. Implement the Reader Monad

instance Monad (Reader r) where
  return = pure
  -- f :: a -> Reader r b
  -- ra:: r -> a
  (Reader ra) >>= f = Reader $ \x -> runReader (f (ra x)) x

-- 2. Rewrite the monadic getDogRM to use your Reader datatype

newtype HumanName = HumanName String
   deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
      dogsName :: DogName
    , dogsAddress :: Address
  } deriving (Eq, Show)
  
getDogRM' :: Reader Person Dog
getDogRM' = do
  person <- ask
  return $ Dog (dogName person) (address person)

main :: IO ()
main = hspec $ do
  describe "The functor of functions is composition" $ do
    it "bip is identical to bloop" $ do
      bip 42 `shouldBe` bloop 42
    it "composition and fmapping are identical" $ do
      let composed = cap . rev $ "kornelia"
      let fmapped = cap <$> rev $ "kornelia"
      composed `shouldBe` fmapped
    it "tupled is identical to tupled' " $ do
      (tupled "kornelia") `shouldBe` (tupled' "kornelia")

  describe "Applicative: feeding an argument to both functions and combining the results" $ do
  --   We’d use this when two functions would share the same input and
  --   we want to apply some other function to the result of those to reach
  --   a final result
    it "bbop is identical to duwop" $ do
      bbop 3 `shouldBe` duwop 3
    it "bbop 1: (+) <$> (*2), composing two functions to get a function of two arguments" $ do
  --   bbop = (+) <$> boop <*> doop
      let doubleThenPlus = (+) <$> (*2)
      doubleThenPlus 20 2 `shouldBe` (*2) 20 + 2
    it "bbop 2: applicatively combine with another function to get a function of one argument" $ do
      bbop 5 `shouldBe` (*2) 5 + (+10) 5 
      
  describe "Monad: functions have this instance, too" $ do
    it "boopDoop" $ do
      boopDoop 1 `shouldBe` (boop 1) + doop 1
    it "binding" $ do
      (boop >>= (+) <$> doop) 1 `shouldBe` boopDoop 1 
      

  describe "Exercise: Reading Comprehensions" $ do
    it "myLiftA2 is identical to liftA2" $ do
      myLiftA2 (+) (Just 32) (Just 42) `shouldBe` liftA2 (+) (Just 32) (Just 42)
--    it "Testing the Applicative instance" $ do
--      rab <*> ra `shouldBe`

  describe "Making a Reader" $ do
  
    it "running a Reader is calling a function on an environment" $ do
      (runReader r1) env `shouldBe` 42
     
    it "asking for the environment" $ do
      let asked = runReader r3 env
      asked `shouldBe` "some environment"
      
      
    where r1 = return 42 :: Reader String Int
    
          r2 :: Reader String String
          r2 = do
            env <- ask
            return env
          
          r3 :: Reader String String
          r3 = ask
          
          r4 :: Reader String String
          r4 = ask >>= return 
          
          env = "some environment"

