import Data.List

myIterator :: (a -> a) -> a -> [a]
myIterator f i = i : go f i
  where go g x = g x : go g (g x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f i = case f i of
  Nothing -> []
  Just (x, y) -> x : myUnfoldr f y

betterIterator :: (a -> a) -> a -> [a]
betterIterator f = myUnfoldr (\x -> Just (x, f x))
