import Data.List (sort)
import Data.Char (toUpper)

import Test.QuickCheck

capitalizeWord :: String -> String
capitalizeWord = map toUpper

twice f = f . f
fourTimes = twice . twice

fs2 x =
  sort x == twice sort x

fs4 x =
  sort x == fourTimes sort x

fc2 x =
  capitalizeWord x == twice capitalizeWord x

fc4 x =
  capitalizeWord x == fourTimes capitalizeWord x

main :: IO ()
main = do
  quickCheck (fs2 :: [Int] -> Bool)
  quickCheck (fs4 :: [Int] -> Bool)
  quickCheck fc2
  quickCheck fc4
