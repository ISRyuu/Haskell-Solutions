import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity x) = f x
  
instance Traversable Identity where
  sequenceA (Identity a) = Identity <$> a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

newtype Constant a b =
  Constant {getConstant :: a} deriving (Eq, Show, Ord)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  sequenceA (Constant x) = pure (Constant x)
  
data Optional a =
    Nada
  | Yep a
  deriving (Show, Eq, Ord)
  
instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = 
    oneof [return Nada, Yep <$> arbitrary]

instance Functor Optional where
  fmap f (Yep x) = Yep $ f x
  fmap _ Nada = Nada

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  sequenceA Nada = pure Nada
  sequenceA (Yep a) = Yep <$> a

data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq, Ord)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x $ fromList xs
  
instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fromList <$> arbitrary

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) =
    (f x) <> (foldMap f xs)
  
instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> (sequenceA xs)

data Three a b c =
  Three a b c
  deriving (Eq, Show, Ord)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  sequenceA (Three a b c) = (Three a b) <$> c

data Three' a b =
  Three' a b b
  deriving (Eq, Show, Ord)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' a b b' ) = (f b) <> (f b')

instance Traversable (Three' a) where
  sequenceA (Three' a b b') = (Three' a) <$> b <*> b'

data S n a = S (n a) a deriving (Show, Eq, Ord)

instance Functor n => Functor (S n) where
  fmap f (S z a) = S (f <$> z) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S z a) = (foldMap f z) <> (f a)

instance Traversable n => Traversable (S n) where
  sequenceA (S z a) = S <$> (sequenceA z) <*> a
  
instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

instance (Applicative n, Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> (pure <$> arbitrary) <*> arbitrary

main :: IO ()
main = do
  let trigger = undefined
  quickBatch $ traversable (trigger :: Identity (Int, Int, [Int]))
  quickBatch $ traversable (trigger :: Constant String (Int, Int, [Int]))
  quickBatch $ traversable (trigger :: Optional (Int, Int, [Int]))
  quickBatch $ traversable (trigger :: List (Int, Int, [Int]))
  quickBatch $ traversable (trigger :: Three Int Int (Int, Int, [Int]))
  quickBatch $ traversable (trigger :: Three' Int (Int, Int, [Int]))    
  quickBatch $ traversable (trigger :: S Maybe (Int, Int, [Int]))    
