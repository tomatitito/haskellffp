module Ch26.Examples where

--newtype  MaybeT m a =
--  MaybeT { runMaybeT :: m (Maybe a) }
--
--instance (Monad m) => Monad (MaybeT m) where
--  -- MaybeT m a >>= a -> MaybeT m b = MaybeT m b
--  (MaybeT ma) >>= f = MaybeT $ do
--    v <- ma
--    case v of
--      Nothing -> return Nothing
--      Just x -> runMaybeT $ f x

-- EitherT
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure (pure x)
  EitherT mef <*> EitherT mea = EitherT $ f' <*> mea
    where f' = fmap (<*>) mef

instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT mea >>= f = EitherT $ do
    v <- mea
    case v of
      Left err -> return $ Left err
      Right v' -> runEitherT (f v')

etf :: EitherT String Maybe (String -> String)
etf = EitherT $ Just $ Right (++ "huhu")

eta :: EitherT String Maybe String
eta = EitherT $ Just $ Right "yupp"

ef :: Either String (String -> String)
ef = Right (++ "huhu")

ea :: Either String String
ea = Right "yes"

ea' :: Either String String
ea' = Left "No"

mf :: Maybe (Either String (String -> String))
mf = Just ef
me :: Maybe (Either String String)
me = Just ea
mf' = fmap (<*>) mf

-- State a = State { runState s -> (a, s) }
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  -- lift over m
  -- apply f to a
  fmap f (StateT sma) = StateT $ \s -> fmap f' (sma s)
    where f' (a, s) = (f a, s)
  -- remember: StateT is a wrapper for a function
  -- fmap is used inside of that function
  -- it is then possible to run that thing using runStateT
  -- at this point, initial state must be provided

-- Example ussage:
-- remember: StateT is a function wrapper
-- in other words, a value of type StateT is a function
-- below is such a function, and further below a value using the function
statet ::Int -> Maybe (Int, Int)
statet i = Just (i, i) --if even i then Just (i, i) else Nothing

x = runStateT (StateT statet) 42

inc :: Int -> Int
inc = (+1)

y = fmap inc $ StateT statet
y' = runStateT y 42

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  StateT smf <*> StateT sma = StateT $ \s -> do
    -- smf :: s -> m (a -> b, s)
    -- sma :: s -> m (a, s)
    -- apply smf to an s to get to m (a -> b, s)
    -- apply sma to an s to get to m (a, s)
    (f, s') <- smf s
    (a, s'') <- sma s'
    return $ (f a, s'')

statetf :: Int -> Maybe (Int -> Int, Int)
statetf i = Just ((+i), i)

xf = StateT statetf
xa = StateT statet
b = runStateT (xf <*> xa) 42

instance Monad m => Monad (StateT s m) where
  return = pure
  StateT sma >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'
    
stBindF :: Int -> StateT Int Maybe Int
stBindF _ = return 666

stimb = runStateT (xa >>= stBindF) 42
