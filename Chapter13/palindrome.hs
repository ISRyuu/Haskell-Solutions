import Control.Monad
import Data.Char


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome."
    False -> putStrLn "Nope"

palindrome' :: IO ()
palindrome' = forever $ do
  line1 <- getLine
  let line2 = fmap toLower $ filter isAlphaNum line1
  case (line2 == reverse line2) of
    True -> putStrLn "It's a palindrome."
    False -> putStrLn "Nope"


main :: IO ()
main = palindrome'
