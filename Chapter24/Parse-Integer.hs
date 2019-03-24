{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Text.Trifecta
import Data.Char
import Data.Word

parseDigit :: Parser Char
parseDigit = do
  oneOf "0123456789" <|> fail "expected: digit"

base10IntegerNeg :: Parser Integer
base10IntegerNeg = do
  char '-'
  d <- base10Integer
  return $ negate d

base10Integer :: Parser Integer
base10Integer = some parseDigit >>= (return . read)

base10IntegerWithNeg :: Parser Integer
base10IntegerWithNeg = base10IntegerNeg <|> base10Integer

main :: IO ()
main = print $ parseString base10IntegerWithNeg mempty "-001345"
