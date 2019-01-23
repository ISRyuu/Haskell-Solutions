module Main where

import Test.QuickCheck
import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Show, Eq)

-- Semigroup is a superclass of Monoid since base-4.11.0.0.
instance Semigroup a => Semigroup (Optional a) where
 (<>) Nada x = x
 (<>) x Nada = x
 (<>) (Only x) (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mappend = (<>)
  mempty = Nada

-- The reason to use warpper is these two exercise are seperate in
-- the book, and to avoid orphan instance, the auther adds a wrapper.
newtype First' a =
  First' {getFirst' :: Optional a}
  deriving (Show, Eq)

instance Semigroup (First' a) where
  (<>) (First' Nada) x = x
  (<>) x _ = x
  
instance Monoid (First' a) where
  mappend = (<>)
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ First' Nada, return $ First' (Only a)]

monoidAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
monoidAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
monoidRightIdentity a = (a <> mempty) == a 


main :: IO ()
main = do
  print $ Nada `mappend` Only (Product 2)
  print $ Only (Sum 3)  `mappend` Only (Sum 2) 
  print $ Only (Sum 2)  `mappend` Nada 
  print $ (Nada :: Optional String) <> Nada
  quickCheck (monoidAssoc :: First' String
                          -> First' String
                          -> First' String
                          -> Bool)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
