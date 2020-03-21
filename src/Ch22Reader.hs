module Ch22Reader where

import Test.Hspec
import Control.Applicative (liftA2)
import Control.Monad (return)
import Data.Char

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


