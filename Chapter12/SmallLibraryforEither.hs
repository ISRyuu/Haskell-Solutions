import Data.List


lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where go (Left a) xs = a : xs
        go _ xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where go (Right a) xs = a : xs
        go _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where go (Right a) (ls, rs) = (ls, a : rs)
        go (Left a) (ls, rs) = (a : ls, rs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just $ f x
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left x) = fl x
either' _ fr (Right x) = fr x

eitherMaybe'' :: (b -> c) -> Either a b -> c
eitherMaybe'' f = either' undefined f
