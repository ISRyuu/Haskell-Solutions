newtype Constant a b =
  Constant {getContant :: a}
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (x <> y)
