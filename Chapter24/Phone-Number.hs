-- Very Messy

import Text.Trifecta
import Text.Printf
import Control.Applicative
  
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Show, Eq)
  
betweenRange :: Int -> Int -> Int -> Bool
betweenRange n l u = n >= l && n <= u

parseRange :: Int -> Int -> Parser Int
parseRange l u = do
  n <- fromInteger <$> decimal
  if betweenRange n l u
    then return n
    else fail $ printf "expected: must be in range %d ~ %d" l u

parseNormal :: Parser PhoneNumber
parseNormal = do
  npa <- parseRange 0 999
  char '-'
  ex  <- parseRange 0 999
  char '-'  
  ln  <- parseRange 0 9999
  return $ PhoneNumber npa ex ln

-- This function is from
-- https://github.com/andrewMacmurray/haskell-book-solutions/blob/master/src/ch24/CanadaPhone.hs
allInOne :: Parser PhoneNumber
allInOne = do
  xs <- some digit
  let a = take 3 xs
      b = take 3 . drop 3 $ xs
      c = drop 6 xs
      correctLength = length xs == 10
      result
        | correctLength = return $ PhoneNumber (read a) (read b) (read c)
        | otherwise     = fail "incorrect length"
  result

parseParentheseFormat :: Parser PhoneNumber
parseParentheseFormat = do
 npa <- char '(' *> (parseRange 0 999) <* char ')'
 skipMany (oneOf " ")
 ex  <- parseRange 0 999
 char '-'
 ln <- parseRange 0 9999
 return $ PhoneNumber npa ex ln

parsePrefixFormat :: Parser PhoneNumber
parsePrefixFormat = do
  try $ do
   digit
   char '-'
  npa <- parseRange 0 999
  char '-'
  ex <- parseRange 0 999
  char '-'
  ln <- parseRange 0 9999
  return $ PhoneNumber npa ex ln

parsePhoneNumber :: Parser PhoneNumber
parsePhoneNumber =
      (try allInOne)
  <|> parseParentheseFormat
  <|> parsePrefixFormat
  <|> parseNormal  
  
main :: IO ()
main = do
  print $ parseString parsePhoneNumber mempty "123-456-7891"
  print $ parseString parsePhoneNumber mempty "(123) 456-7891"
  print $ parseString parsePhoneNumber mempty "1-123-456-7891"
  print $ parseString parsePhoneNumber mempty "1234567891"
