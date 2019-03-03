import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
  
data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq, Ord)

instance Eq a => EqProp (Tree a) where (=-=) = eq

-- see following links for more info about generate a random tree.
-- https://stackoverflow.com/questions/15959357/quickcheck-arbitrary-instances-of-nested-data-structures-that-generate-balanced
-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
treeGen :: Arbitrary a => Int -> Gen (Tree a)
treeGen 0 = Leaf <$> arbitrary
treeGen n
  | n > 0 = oneof [Leaf <$> arbitrary,
                   liftA3 Node child (arbitrary) child]
  where child = treeGen (n `div` 2)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [return Empty, sized treeGen]
  
instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node ls a rs) = Node (fmap f ls) (f a) (fmap f rs)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node ls a rs) = (foldMap f ls) <> f a <> (foldMap f rs)

instance Traversable Tree where
  sequenceA Empty = pure Empty
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node ls a rs) = liftA3 Node (sequenceA ls) a (sequenceA rs)
  
  
main :: IO ()
main = do
  let trigger = undefined
  quickBatch $ traversable (trigger :: Tree (Int, Int, [Int]))
