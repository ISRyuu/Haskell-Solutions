{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random


newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ 
    \r -> let (a, s) = g r
          in (f a, s)
   
-- https://stackoverflow.com/questions/45154169/applicative-instance-for-state-order-of-data-flow
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $
    \s ->
      let (f', s') = f s
          (a', s'') = g s'
      in (f' a, s'')

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (>>=) (Moi f) g = Moi $
    \s ->
      let (a, s') = f s
          (Moi f') = g a
          in f' s'

  
main :: IO ()
main = undefined
