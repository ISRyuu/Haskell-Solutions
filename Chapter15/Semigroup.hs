{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.QuickCheck
import Data.Monoid

semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

monoidLeftIdentiy :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentiy x = (mempty <> x == x)

monoidRightIdentiy :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentiy x = (x <> mempty == x)

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Monoid Trivial where
  mempty = Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2

newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity String
                   -> Identity String
                   -> Identity String
                   -> Bool

-- 3

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two String [Int]
              -> Two String [Int]
              -> Two String [Int]
              -> Bool

-- 4

data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc = Three String String String
               -> Three String String String
               -> Three String String String
               -> Bool

-- 5

data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four x y z w) <> (Four x' y' z' w') = Four (x <> x') (y <> y') (z <> z') (w <> w')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary    
    return $ Four a b c d

type FourAssoc = Four String String String String
               -> Four String String String String
               -> Four String String String String
               -> Bool

-- 6

newtype BoolConj =
  BoolConj Bool deriving (Show, Eq)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  
-- 7

newtype BoolDisj =
  BoolDisj Bool deriving (Show, Eq)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

instance Semigroup BoolDisj where
  (BoolDisj False) <>(BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  
-- 8

data Or a b =
    Fst a
  | Snd b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) =>  Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> (Snd a) = Snd a
  _ <> x = x

-- 9

newtype Combine a b =
  Combine { unCombine :: (a -> b)} deriving Arbitrary

-- Terrible instance
instance Show (Combine a b) where
  show = \_ -> "Combine a b"

instance Semigroup b => Semigroup (Combine a b) where
  x <> y = Combine $ \t -> (unCombine x $ t) <> (unCombine y $ t)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine id

combineAssoc :: (Eq b, Semigroup b)
             => (Combine a b)
             -> (Combine a b)
             -> (Combine a b)
             -> a
             -> Bool
             
combineAssoc f g z x =
  (unCombine f $ x) <> (unCombine (g <> z) $ x)
  == (unCombine (f <> g) $ x) <> (unCombine z $ x)

combineLeftIdentity f x =
  (unCombine (f <> mempty) $ x) == x

-- 10

newtype Comp a =
  Comp { unComp :: (a -> a) } deriving Arbitrary

-- Terrible instance, as usual.
instance Show (Comp a) where
  show = \_ -> "Comp a b"

instance Semigroup a => Semigroup (Comp a) where
  x <> y = Comp $ \t -> (unComp x $ t) <> (unComp y $ t)

compAssoc :: (Eq a, Semigroup a)
          => (Comp a)
          -> (Comp a)
          -> (Comp a)
          -> a
          -> Bool
compAssoc f g z x =
  (unComp f $ x) <> (unComp (g <> z) $ x)
  == (unComp (f <> g) $ x) <> (unComp z $ x)

-- 11

data Validation a b =
  Failure' a | Success' b
  deriving  (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
  (Failure' a) <> (Failure' b) = Failure' (a <> b)
  (Success' a) <> (Success' b) = Success' (a <> b)
  x <> (Success' _) = x
  (Success' _) <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Failure' a, return $ Success' b]

-- 12

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success' a)) <> (AccumulateRight (Success' b)) =
    (AccumulateRight $ Success' (a <> b))
  x <> (AccumulateRight (Failure' _)) = x
  (AccumulateRight (Failure' _)) <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ AccumulateRight (Failure' a),
           return $ AccumulateRight (Success' b)]

-- 13

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
  (AccumulateBoth x) <> (AccumulateBoth x') =
   AccumulateBoth (x <> x')

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ AccumulateBoth (Failure' a),
           return $ AccumulateBoth (Success' b)]


main :: IO ()
main = do
  putStrLn "1 semigroup"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  putStrLn "1 monoid"
  quickCheck (monoidLeftIdentiy :: Trivial -> Bool)
  quickCheck (monoidRightIdentiy :: Trivial -> Bool)
  putStrLn "2 semigroup"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  putStrLn "2 monoid"
  quickCheck (monoidLeftIdentiy :: Identity String -> Bool)
  quickCheck (monoidRightIdentiy :: Identity String -> Bool)
  putStrLn "3 semigroup"
  quickCheck (semigroupAssoc :: TwoAssoc)
  putStrLn "3 monoid"
  quickCheck (monoidLeftIdentiy :: Two String String -> Bool)
  quickCheck (monoidRightIdentiy :: Two String String -> Bool)
  putStrLn "4 semigroup"
  quickCheck (semigroupAssoc :: ThreeAssoc)
  putStrLn "5 semigroup"  
  quickCheck (semigroupAssoc :: FourAssoc)
  putStrLn "6 semigroup"
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  putStrLn "4 monoid"
  quickCheck (monoidLeftIdentiy :: BoolConj -> Bool)
  quickCheck (monoidRightIdentiy :: BoolConj -> Bool)
  putStrLn "7 semigroup"  
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  putStrLn "5 monoid"
  quickCheck (monoidLeftIdentiy :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentiy :: BoolDisj -> Bool)
  putStrLn "8 semigroup"  
  quickCheck (semigroupAssoc :: Or Int Char
                             -> Or Int Char
                             -> Or Int Char
                             -> Bool)
  putStrLn "9 semigroup"
  quickCheck (combineAssoc :: Combine Int String
                           -> Combine Int String
                           -> Combine Int String
                           -> Int
                           -> Bool)
  putStrLn "6 monoid"
  quickCheck (monoidLeftIdentiy :: Combine Int String -> Bool)
  quickCheck (monoidRightIdentiy :: Combine Int String -> Bool)
  putStrLn "10 semigroup"    
  quickCheck (compAssoc :: Comp String
                        -> Comp String
                        -> Comp String
                        -> String
                        -> Bool)
  putStrLn "11 semigroup"    
  quickCheck (semigroupAssoc :: Validation String String
                             -> Validation String String
                             -> Validation String String
                             -> Bool)
  putStrLn "12 semigroup"    
  quickCheck (semigroupAssoc :: AccumulateRight String String
                             -> AccumulateRight String String
                             -> AccumulateRight String String
                             -> Bool)
  putStrLn "13 semigroup"    
  quickCheck (semigroupAssoc :: AccumulateBoth String String
                             -> AccumulateBoth String String
                             -> AccumulateBoth String String
                             -> Bool)
