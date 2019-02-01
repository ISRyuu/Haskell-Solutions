import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ i Nil = i
fold f i (Cons x xs) = f x (fold f i xs)

concat' :: List (List a) -> List a
concat' xs = fold (<>) Nil xs

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ f <$> xs

instance Semigroup (List a) where
  (<>) Nil x = x
  (<>) x Nil = x
  (<>) (Cons x xs) ys = Cons x (xs <> ys)
  
instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = (fmap f xs) <> (fs <*> xs) 

instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fromList <$> arbitrary
  
main :: IO ()
main = do
 let trigger = undefined :: List (String, String, Int)
 quickBatch $ applicative trigger
