import Data.List

replaceThe :: String -> String
replaceThe [] = []
replaceThe s@(x:xs)
  | x == ' ' = x : replaceThe xs
  | otherwise =
    (if word == "the" then "a" else word) ++ replaceThe rest
  where word = takeWhile (/=' ') s
        rest = dropWhile (/=' ') s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go False 0 . words
  where
    go _ vs [] = vs
    go _ vs ("the":xs) = go True vs xs
    go t vs (x:xs)
      | t && head x `elem` "aeiou" = go False (1 + vs) xs
      | otherwise = go False vs xs

countVowels :: String -> Integer
countVowels =
  foldr (\c n -> if c `elem` "aeiou" then n+1 else n) 0
