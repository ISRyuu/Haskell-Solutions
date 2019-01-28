import Data.Functor
import Test.QuickCheck.Function
import Test.QuickCheck


propFunctorIdentity :: (Eq (f a), Functor f) => f a -> Bool
propFunctorIdentity x = (fmap id x) == (id x)

propFunctorCompose :: (Eq (f c), Functor f)
                   => f a
                   -> Fun a b
                   -> Fun b c
                   -> Bool
propFunctorCompose x (Fun _ g) (Fun _ f) =
  fmap (f . g) x == fmap f (fmap g x)

-- 1

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- 4

data Three a b c = Three a b c deriving (Eq, Show) 

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary    
    return $ Three a b c

-- 5

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')
 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary    
    return $ Three' a b b'

-- 6
-- There is not much difference between Three and Four.

-- 7

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)
 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary    
    b <- arbitrary    
    return $ Four' a a' a'' b

-- No, there is no Functor instance for Trivial.
 
main :: IO ()
main = do
  quickCheck (propFunctorIdentity :: Identity Int -> Bool)
  quickCheck (propFunctorCompose :: Identity String
                                 -> Fun String String
                                 -> Fun String String
                                 -> Bool)
  quickCheck (propFunctorIdentity :: Pair Int -> Bool)
  quickCheck (propFunctorCompose :: Pair String
                                 -> Fun String String
                                 -> Fun String String
                                 -> Bool)
  quickCheck (propFunctorIdentity :: Two Int Int -> Bool)
  quickCheck (propFunctorCompose :: Two String String
                                 -> Fun String String
                                 -> Fun String String
                                 -> Bool)
  quickCheck (propFunctorIdentity :: Three Int Int Int -> Bool)
  quickCheck (propFunctorCompose :: Three String String Int
                                 -> Fun Int Char
                                 -> Fun Char String
                                 -> Bool)
  quickCheck (propFunctorIdentity :: Three' Int Int -> Bool)
  quickCheck (propFunctorCompose :: Three' String Int
                                 -> Fun Int Char
                                 -> Fun Char String
                                 -> Bool)
  quickCheck (propFunctorIdentity :: Four' Int Int -> Bool)
  quickCheck (propFunctorCompose :: Four' String Int
                                 -> Fun Int Char
                                 -> Fun Char String
                                 -> Bool)
