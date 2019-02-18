import Data.Maybe
import Data.Foldable
import Data.Monoid (
  Sum(..),
  Product(..),
  Any(..)
  )

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . (foldMap Sum)

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . (foldMap Product)

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . (foldMap $ Any . (==x))

newtype Min a = Min {getMin :: Maybe a} deriving (Show, Eq, Ord)

instance Ord a => Semigroup (Min a) where
  x <> (Min Nothing) = x
  (Min Nothing) <> x = x
  (Min a) <> (Min b) = Min $ min <$> a <*> b

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing

newtype Max a = Max {getMax :: Maybe a} deriving (Show, Eq, Ord)

instance Ord a => Semigroup (Max a) where
  x <> (Max Nothing) = x
  (Max Nothing) <> x = x
  (Max a) <> (Max b) = Max $ max <$> a <*> b
    
instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getMin . (foldMap (Min . Just))

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax . (foldMap (Max . Just))

null' :: (Foldable t) => t a -> Bool
null' = \x -> length' x == 0

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\x -> Sum 1)

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (:[])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

main :: IO ()
main = do
  print $ sum' [1,2,3,4]
  print $ product' [1,2,3,4]    
  print $ maximum' [1,2,3,4]
  print $ minimum' [1,2,3,4]
  print $ toList'  (Just 1)
  print $ fold' $ map Sum [1,2,3]
  print $ foldMap' Sum [1,2,3]
