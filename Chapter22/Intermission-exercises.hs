import Data.Char

-- short exercise, Warming Up, Page 1285

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

-- page 1295, ask

newtype Reader' r a =
  Reader' {getReader :: r -> a}

ask' :: Reader' a a
ask' = Reader' id

-- Exercise: Reading Comprehensive

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader' r a
asks f = Reader' f

instance Functor (Reader' a) where
  fmap f (Reader' r) = Reader' (f . r)

instance Applicative (Reader' a) where
  pure = Reader' . const
  (<*>) (Reader' f) (Reader' g) = Reader' $ \r -> f r (g r)

-- Exercise: Reader Monad

instance Monad (Reader' r) where
  return = pure
  (Reader' m) >>= f = Reader' $ \r -> (getReader $ f (m r)) r

main :: IO ()
main = undefined
