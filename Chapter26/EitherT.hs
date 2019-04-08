import Control.Applicative

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
