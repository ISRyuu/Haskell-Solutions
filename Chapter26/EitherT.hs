{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad

newtype EitherT e m a =
  EitherT {runEitherT :: m (Either e a)} 

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . pure) x
  (<*>) (EitherT f) (EitherT e) = EitherT $ (<*>) <$> f <*> e

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f =
    EitherT $ do
    a <- ma
    case a of
      (Left x) -> return $ Left x
      (Right x) -> runEitherT (f x)

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT x) = EitherT $ swapEither <$> x

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mab) = mab >>= either f g

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . liftM Right
