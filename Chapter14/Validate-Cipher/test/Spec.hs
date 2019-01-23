import Data.Char
import Test.QuickCheck
import VigenereCipher

prop_cipher :: String -> String -> Bool
prop_cipher x y = (vigenereDecipher fx (vigenereCipher fx fy)) == fy
  where fx = map toUpper $ filter isAlpha $ filter ((<256) . ord) x
        fy = filter ((<256) . ord) y

main :: IO ()
main = do
  quickCheck prop_cipher
  
