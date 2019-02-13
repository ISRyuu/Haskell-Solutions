import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg
  
instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg
  
instance EqProp (Nope a) where (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' x) = Right' x
  fmap f (Left' x) = Left' $ f x

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Left' f) (Left' x) = Left' $ f x
  (<*>) (Right' f) _ = Right' f
  (<*>) _ (Right' f) = Right' f

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Left' x) f = f x
  (>>=) (Right' x) _ = Right' x

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Left' a, return $ Right' b]

newtype Identity a = Identity a deriving (Show, Eq, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

instance Semigroup (List a) where
  (<>) x Nil = x
  (<>) Nil x = x
  (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = \x -> Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = fmap f xs <> (fs <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil f = Nil
  (>>=) (Cons x xs) f = (f x) <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fromList <$> arbitrary

instance Eq a => EqProp (List a) where (=-=) = eq
  
main :: IO ()
main = do
  let trigger1 = undefined :: Nope (String, String, String)
  quickBatch $ monad trigger1
  let trigger2 = undefined :: PhhhbbtttEither String (String, String, String)
  quickBatch $ monad trigger2
  let trigger3 = undefined :: Identity (String, String, String)
  quickBatch $ monad trigger3
  let trigger4 = undefined :: List (String, String, String)
  quickBatch $ monad trigger4
