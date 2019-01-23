import Data.List (sort)
import Data.Char (toUpper)

import Test.QuickCheck

data Fool =
    Fulse
  | Fure
  deriving (Show, Eq)

instance Arbitrary Fool where
  arbitrary = oneof [return Fulse, return Fure]

genFoolWithProbility :: Gen Fool
genFoolWithProbility = frequency [(2, return Fulse), (1, return Fure)]


main :: IO ()
main = do
  sample (arbitrary :: Gen Fool)
  sample genFoolWithProbility
