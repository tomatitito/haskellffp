module Ch23.Understate where

import Control.Monad.Trans.State
import Control.Monad (replicateM)
import System.Random

initialize :: Int -> (Int, Int)
initialize i = (i, i)

feed :: Int -> State Int Int
feed = return
--feed' i = state $ \i ->
--  (if even (i + 1) then (i + 1, i + 1) else (i + 1, i))

-- declarative way of doing things:
-- evens describes what to do without actually having state lying around
-- only when you run the state processor (using runState) the state is manifested
-- this is done by applying it to the initial state

--       State s   val
evens :: State Int Int
evens = do
  old <- get
  -- do stuff to the old state here
  let new = old + 1
  if even new
    then state $ const (new, new)
    else state $ \_ -> (old, new)
  -- return 1 -- 1 is the val
 
randomInt :: State StdGen Int
randomInt = state $ do
  (i, g) <- random 
  return (i, g)
  
randomEvens :: State Int Int
randomEvens = do
  old <- get
  let gen = mkStdGen 42
      (offset, newGen) = randomR (1, 1000) gen :: (Int, StdGen)
      new = offset
  if even new
    then state $ const (new, new)
    else state $ const (old, new)

runUnderstate :: IO ()
--runUnderstate = putStrLn $ show ( execState evens 2)
runUnderstate = do
  let st = runState (replicateM 10 randomEvens ) 42
--  let r = randomR (1, 10000) (mkStdGen 42) :: (Int, StdGen)
  putStrLn $ show st
