data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
  Nothing -> Leaf
  Just (nl, v, nr) -> Node (unfold f nl) v (unfold f nr)

treeBuilder :: Integer -> BinaryTree Integer 
treeBuilder n = unfold (go n) 0
  where go n x
          | x == n || n < 0 = Nothing
          | otherwise = Just (x+1, x, x+1)
  
