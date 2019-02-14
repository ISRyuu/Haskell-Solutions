import Control.Monad (join)

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
--l2 f x y = f <$> x <*> y

-- l2 f x y = do
--   a <- x
--   b <- y
--   return $ f a b

l2 f x y = x >>= (\t -> y >>= (\z -> return $ f t z))
                    

a :: Monad m => m a -> m (a -> b) -> m b
-- a = flip (<*>)

-- a mx mf = do
--   f <- mf  
--   x <- mx
--   return $ f x

a x f = f >>= (\g -> x >>= (\t -> return $ g t))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh [] _ = return []
-- meh (x:xs) f = do
--   let rs = meh xs f
--   r <- rs
--   h <- f x 
--   return $ h:r
  
meh [] _ = return []
meh (x:xs) f = (meh xs f) >>= (\ts -> (f x) >>= (\t -> return $ t:ts))

flipType :: (Monad m) => [m a] -> m [a]
flipType = \x -> meh x id 
