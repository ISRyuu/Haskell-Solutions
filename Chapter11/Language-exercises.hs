import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord (x:xs)
  | x == ' '  = x : capitalizeWord xs
  | otherwise = toUpper x : xs
capitalizeWord x = x

sentences :: String -> [String]
sentences [] = []
sentences ('.':xs) = "." : sentences xs
sentences p = takeWhile (/='.') p : sentences (dropWhile (/='.') p)

capitalizeParagraph' :: String -> String
capitalizeParagraph' = concat . map capitalizeWord . sentences
