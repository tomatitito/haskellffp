{-# LANGUAGE InstanceSigs #-}


module Reader where

import Test.Hspec
import Control.Monad.Reader
import Control.Applicative (liftA2)
import Control.Monad (return)
import Data.Char


--newtype Reader r a =
--  Reader { runReader :: r -> a }
--
--instance Functor (Reader r) where
--  fmap :: (a -> b) -> Reader r a -> Reader r b
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

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 fun x y =
  fun <$> x <*> y

--instance Applicative (Reader r) where
--  pure :: a -> Reader r a
--  pure a = Reader $ \_ -> a
--
--  (<*>) :: Reader r (a -> b)
--        -> Reader r a
--        -> Reader r b
--  (Reader rab) <*> (Reader ra) =
--    Reader $ \r -> rab ra


main :: IO ()
main = hspec $ do
  describe "The functor of functions is composition" $ do
    it "bip is identical to bloop" $ do
      bip 42 `shouldBe` bloop 42
    it "bbop is identical to duwop" $ do
      bbop 3 `shouldBe` duwop 3
    it "composition and fmapping are identical" $ do
      let composed = cap . rev $ "kornelia"
      let fmapped = cap <$> rev $ "kornelia"
      composed `shouldBe` fmapped
    it "tupled is identical to tupled' " $ do
      (tupled "kornelia") `shouldBe` (tupled' "kornelia")

  describe "Exercise: Reading Comprehensions" $ do
    it "myLiftA2 is identical to liftA2" $ do
      myLiftA2 (+) (Just 32) (Just 42) `shouldBe` liftA2 (+) (Just 32) (Just 42)

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

