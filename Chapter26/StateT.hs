{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Class
import Control.Monad
import Control.Applicative

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
    fmap (\(x, s) -> (f x, s)) $ sma s

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) (StateT smab) (StateT sma) =
    StateT $ \s -> do
      (f, s') <- smab s
      (a, s'') <- sma s'
      return (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) (StateT sma) f =
    StateT $ \s -> do
      (a, s') <- sma s
      runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ \s -> (flip (,) $ s) <$> ma
