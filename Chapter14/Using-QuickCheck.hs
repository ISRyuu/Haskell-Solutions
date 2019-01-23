{-# LANGUAGE GeneralisedNewtypeDeriving #-}
  
import Test.QuickCheck
import Test.Hspec
import Data.List (sort)

half x = x / 2

halfIdentity = (*2) . half

prop_half :: Double -> Bool
prop_half x = x == (halfIdentity x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_sort :: (Ord a) => [a] -> Bool
prop_sort = listOrdered . sort 

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> a -> Bool
plusCommutative x y z =
  x + y == y + x

mulAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
mulAssociative x y z =
  x * (y * z) == (x * y) * z

mulCommutative :: (Eq a, Num a) => a -> a -> a -> Bool
mulCommutative x y z =
  x * y == y * x

prop_quotRem :: (Integral a ) => a -> a -> Bool
prop_quotRem _ 0 = True
prop_quotRem x y = (quot x y)*y + (rem x y) == x

prop_divMod :: (Integral a ) => a -> a -> Bool
prop_divMod _ 0 = True
prop_divMod x y = (div x y)*y + (mod x y) == x

powerCommutative :: (Integral a) => a -> a -> Bool
powerCommutative x y =
  x ^ y == y ^ x

powerAssociative :: (Integral a) => a -> a -> a -> Bool
powerAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

prop_reverse x = (reverse . reverse) x == id x

prop_readShow x = (read (show x)) == x

-- define a newtype Func a b because we need to provide an Show instance
-- for type (a -> b), otherwise `quickcheck` will blame "no instance for
-- Show (a -> b)". We also don't want an orphan instance, and that comes
-- newtype.
newtype Func a b = Func (a -> b) deriving (Arbitrary)

-- Terrible instance, but meet the demand.
instance Show (Func a b) where
  show = \_ -> "Func a b"

prop_dollarSign :: Eq b => Func a b -> a -> Bool
prop_dollarSign (Func f) x = (f $ x) == (f x)

prop_composition :: Eq c => Func b  c -> Func a b -> a -> Bool
prop_composition (Func f) (Func g) x = ((f . g) x) == (f (g x))

prop_foldAndAppend x y = foldr (:) x y == (++) x y

main :: IO ()
main = do
  -- 1
  putStrLn "1"
  quickCheck prop_half
  -- 2
  putStrLn "2"  
  quickCheck (prop_sort :: [Integer] -> Bool)
  -- 3
  putStrLn "3"  
  quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (plusCommutative :: Int -> Int -> Int -> Bool)
  -- 4
  putStrLn "4"
  quickCheck (mulAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (mulCommutative :: Int -> Int -> Int -> Bool)
  -- 5
  putStrLn "5"  
  quickCheck (prop_quotRem :: Int -> Int -> Bool)
  quickCheck (prop_divMod :: Int -> Int -> Bool)
  -- 6
  putStrLn "6"
  quickCheck (powerAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (powerCommutative :: Int -> Int -> Bool)
  -- 7
  putStrLn "7"  
  quickCheck (prop_reverse :: [Int] -> Bool)
  -- 8
  putStrLn "8"  
  quickCheck (prop_dollarSign :: Func Int Int -> Int -> Bool)
  quickCheck (prop_composition :: Func Int Char -> Func Int Int -> Int -> Bool)
  -- 9
  putStrLn "9"  
  quickCheck (prop_foldAndAppend :: String -> String -> Bool)
  quickCheck ((\x -> (foldr (++) [] x) == (concat x)) :: [String] -> Bool)
  -- 10
  putStrLn "10"  
  quickCheck ((\n xs -> length (take n xs) == n) :: Int -> [Int] -> Bool)
  -- 11
  putStrLn "11"
  quickCheck (prop_readShow :: String -> Bool)

