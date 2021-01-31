{-# LANGUAGE InstanceSigs #-}
module Ch25.Twinplicative where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ pure $ pure x

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose fgh <*> Compose fgx = 
  -- if we had fh and fx, we could just do fh <+> fx
  -- using f's Applicative instance
  -- but we have fgh and fgx
  -- fgh is short for f (g (a -> b))
  -- so we need to somehow get to f (g a -> g b)
  -- then, we can just use f's Applicative instance
  -- how? we partially apply (<*>) to g (a -> b)
  -- by fmapping over the f
    let fgagb = fmap (<*>) fgh
  -- then we can apply this to fgx
  -- reminder: the types are now f (g a -> g b) and f (g a)
        fgb = fgagb <*> fgx
    in Compose fgb
  -- or equivalently
  --   Compose $ (fmap (<*>) fgh) <*> fgx

-- Example usage:
c1 = Compose $ [Just (+1)]
c2 = Compose $ [Just 2]
c3 = c1 <*> c2
