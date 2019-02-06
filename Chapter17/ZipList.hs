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

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Show, Eq) 

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Semigroup (ZipList' a) where
  (ZipList' x) <> (ZipList' y) = ZipList' (x <> y)
  
instance Monoid (ZipList' a) where
  mempty = ZipList' mempty

instance Functor ZipList' where
  fmap f (ZipList' x) = ZipList' $ fmap f x

-- This is my original solution, which has two error.
--
-- One is `pure x` of ZipList should be an infinite list `ZipList [x,x,x,x,x...]`,
-- instead of `ZipList [x]`, see
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#functors-redux
-- (search `ZipList` in that page).
--
-- The other is the implementation of (<*>) will cause an stackoverflow when `quickBatch applicative`
-- tests homomorphism law. I haven't figured it out yet.
--
-- instance Applicative ZipList' where
--   pure x = ZipList' $ pure x
--   (ZipList' Nil) <*> _ = ZipList' Nil
--   _ <*> (ZipList' Nil) = ZipList' Nil
--   (ZipList' (Cons f fs)) <*> (ZipList' (Cons y ys )) =
--     (ZipList' $ pure (f y)) <> (ZipList' fs <*>  ZipList' ys)

instance Applicative ZipList' where
  pure x = ZipList' $ fromList (repeat x)
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' x) <*> (ZipList' y) =
    ZipList' $ zipWith' (\f x -> f x) x y

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f Nil _ = Nil
zipWith' f _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

  
main :: IO ()
main = do
 let trigger = undefined :: List (String, String, Int)
 quickBatch $ applicative trigger
 let trigger = undefined :: (ZipList' (String, String, Int))
 quickBatch $ applicative trigger 
