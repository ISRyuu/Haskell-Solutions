import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
    First a
  | Second b
  deriving (Show, Eq)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  (First x) <*> _ = First x
  (Second x) <*> (First y) = First y
  (Second f) <*> (Second y) = Second (f y)

instance Monad (Sum a) where
  return = pure
  (First x) >>= _  = First x
  (Second x) >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: Sum Int (String, String, String)
  quickBatch $ monad trigger
