{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
  Functor (Compose f g) where
  fmap f (Compose fg) = Compose $ (fmap . fmap) f fg

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure a = Compose $ (pure . pure) a
  
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (<*>) (Compose f) (Compose a) =
    Compose $ (<*>) <$> f <*> a

main :: IO ()
main = do
  print $ (Compose [Just (+1)]) <*> (Compose [Just 1, Just 2, Nothing])
