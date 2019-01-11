import Data.Char

capitalizeWords :: String -> [(String, String)]
capitalizeWords =  map (\w@(x:xs) -> (toUpper x:xs, w)) . words

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' (x:xs) y = x `elem` y && isSubsequenceOf' xs yb
