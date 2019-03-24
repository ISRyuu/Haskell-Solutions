{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
  
import Data.Aeson
import Data.ByteString as BS 
import Data.ByteString.Lazy as LBS
import Text.RawString.QQ
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Data.Scientific (floatingOrInteger)

sectionJson :: LBS.ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
"whatisit": {"red": "intoothandclaw"}
}
|]

data TestData =
  TestData {
    section :: Host
  , what :: Color
  } deriving (Eq, Show)

newtype Host = Host String deriving (Show, Eq)

data Color =
    Red String
  | Yellow String
  | Blue String
  deriving (Show, Eq)

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ =
    fail "Expected an oject for TestData"

instance FromJSON Host where
  parseJSON (Object v) =
    Host <$> v .: "host"
  parseJSON _ =
    fail "Expected an object for Host"
  
instance FromJSON Color where
  parseJSON (Object v) =
        (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ =
    fail "Expected an object for Color"

data NumberOrString =
    Numba Integer
  | Stringy Text
  deriving (Show, Eq)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _) -> fail "Must be integral number."
      (Right a) -> return $ Numba a
  parseJSON (String a) = return $ Stringy a
  parseJSON _ = fail "Must be String or Integer"

dec :: LBS.ByteString
    -> Maybe NumberOrString
dec = decode

eitherDec :: LBS.ByteString
          -> Either String NumberOrString
eitherDec = eitherDecode

main :: IO ()
main = do
  let blah :: Maybe Value
      blah = decode sectionJson
  print blah
