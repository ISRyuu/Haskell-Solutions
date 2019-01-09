import Data.Char

vigenereCipher :: String -> String -> String
vigenereCipher [] p = p
vigenereCipher c p =
  map chr $ zipWith go (toOffset c) (map ord p)
  where
    go a b
      | isLower (chr b) = roundOffset b a (ord 'a') (ord 'z')
      | isUpper (chr b) = roundOffset b a (ord 'A') (ord 'Z')
      | isAlphaNum (chr b) = roundOffset b a (ord '0') (ord '9')
      | otherwise = b
    toOffset z = cycle $ map ((subtract $ ord 'A') . ord) z
    roundOffset x o l u = (x + o - l) `mod` (u - l + 1) + l

vigenereDecipher :: String -> String -> String
vigenereDecipher [] p = p
vigenereDecipher c p = vigenereCipher rc p
  where rc = map (chr . (base+) . (base-) . ord) c
        base = ord 'A'
