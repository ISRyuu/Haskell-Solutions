import Text.Trifecta
import Data.Ratio
import Control.Applicative

type IntegerOrFraction =
  Either Integer Rational

parseFraction :: Parser Rational
parseFraction = do
  a <- integer
  char '/'
  b <- integer
  case b of
    0 -> fail "divide by zero error"
    _ -> return (a % b)

parseIntegerOrFraction :: Parser IntegerOrFraction
parseIntegerOrFraction = (try $ Right <$> parseFraction) <|> (Left <$> decimal)

main :: IO ()
main = do
  print $ parseString parseIntegerOrFraction mempty "123 / 23"
  print $ parseString parseIntegerOrFraction mempty "123,"
  print $ parseString parseIntegerOrFraction mempty "xx" 
