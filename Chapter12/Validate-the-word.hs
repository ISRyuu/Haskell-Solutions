import Data.Char (isAlpha)

newtype Word' =
  Word' String
  deriving (Show, Eq)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord x = if vs > cs then Nothing else Just (Word' x)
  where
    (vs, cs) = foldr countVowelsAndConsonants (0, 0) x
    countVowelsAndConsonants c (n1, n2) 
      | c `elem` vowels = (n1 + 1, n2)
      | isAlpha c = (n1, n2 + 1)
      | otherwise = (n1, n2)
