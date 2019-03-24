{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Text.Trifecta
import Data.Word

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

parseAddrField :: Parser Word32
parseAddrField = do
  a <- decimal
  if a >= 0 && a <= 255
    then return $ fromInteger a
    else fail "illegal range."

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  f1 <- parseAddrField
  char '.'
  f2 <- parseAddrField
  char '.'
  f3 <- parseAddrField
  char '.'
  f4 <- parseAddrField
  return $ IPAddress $ snd $ foldr go (1, 0) [f1, f2, f3, f4]
  where go x (m, s) = (m*256, x*m+s)
  
main :: IO ()
main = print "xx"
