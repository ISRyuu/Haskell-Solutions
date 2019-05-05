{-# LANGUAGE Strict #-}

module Main where

data List a =
  Nil |
  Cons a (List a)
  deriving (Show)

take' n _ | n <= 0  = Nil
take' _ Nil         = Nil
take' n (Cons x xs) = (Cons x (take' (n-1) xs))

map' _ Nil = Nil
map' f (Cons x xs) = (Cons (f x) (map' f xs))

repeat' x = xs where xs = (Cons x xs)

-- This wont't block because Strict won't change behaviors of [].
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#modularity
repeat'' x = xs where xs = ((:) x xs)

main :: IO ()
main = do
  print $ take' 10 $ map' (+1) (repeat' 1)
