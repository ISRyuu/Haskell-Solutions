import Control.Applicative

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ (pure . pure) x
  (<*>) (ReaderT f) (ReaderT g) =
    ReaderT $ (<*>) <$> f <*> g

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) (ReaderT rma) f =
    ReaderT $ (\r -> rma r >>= (\x -> (runReaderT . f) x r))

-- Textbook's Version
-- instance Monad m => Monad (ReaderT r m) where
--   return = pure
--   (>>=) (ReaderT rma) f =
--     ReaderT $ \r -> do
--     a <- rma r
--     runReaderT (f a) r
    