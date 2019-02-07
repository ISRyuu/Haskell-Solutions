import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation' e a =
    Failure' e
    | Success' a
  deriving (Eq, Show)

instance Functor (Validation' e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' x) = Success' (f x)
  
instance Semigroup e => Applicative (Validation' e) where
  pure = Success'
  (Failure' x) <*> (Failure' y) = Failure' $ x <> y
  (Success' f) <*> (Success' x) = Success' $ f x
  (Failure' x) <*> _ = Failure' x
  _ <*> (Failure' x) = Failure' x

instance (Arbitrary e, Arbitrary s) => Arbitrary (Validation' e s) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary    
    oneof [return $ Failure' a, return $ Success' b]

instance (Eq e, Eq x) => EqProp (Validation' e x) where (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: Validation' String (String, String, String)
  quickBatch $ applicative trigger
