import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes

data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty

data Two a b =
  Two a b
  
instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c ) = f c

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

data Four' a b =
  Four' a b b b
  
instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = (f b) <> (f b') <> (f b'')

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if (f x) then pure x else mempty)
