{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
  
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

headerEx :: ByteString
headerEx = "[blah]"

assignmentEx :: ByteString
assignmentEx = "woot=1"

newtype Header =
  Header String
  deriving (Show, Eq, Ord)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

type Name = String
type Value = String
type Assignment = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  value <- some $ noneOf "\n"
  skipEOL
  return (name, value)

skipEOL :: Parser ()
skipEOL = skipMany $ oneOf "\n"

commentEx :: ByteString
commentEx = "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
  "; blah\n; woot\n  \n;hah"

skipComments :: Parser ()
skipComments = skipMany $ do
  char ';' <|> char '#'
  skipMany $ noneOf "\n"
  skipEOL

sectionEx :: ByteString
sectionEx = [r|
; ignore me
[states]


Chris=Texas|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw


[whatisit]
red=intoothandclaw
|]

data Section =
  Section Header Assignment
  deriving (Show, Eq)

newtype Config =
  Config (Map Header Assignment)
  deriving (Show, Eq)

skipWhiteSpace :: Parser ()
skipWhiteSpace = skipMany $ oneOf " \n"

parseSection :: Parser Section
parseSection = do
  skipWhiteSpace
  skipComments
  h <- parseHeader
  skipWhiteSpace
  assignment <- some parseAssignment
  return $ Section h $ M.fromList assignment

rollUp :: Section
       -> Map Header Assignment
       -> Map Header Assignment
rollUp (Section h a) m =
  M.insert h a m

parseIni :: Parser Config
parseIni = do
  secs <- some parseSection
  let mapOfSections =
        foldr rollUp M.empty secs
  return $ Config mapOfSections

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
    let m = parseByteString parseAssignment mempty assignmentEx
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just ("woot", "1")

  describe "Header Parsing" $
    it "can parse a simple header" $ do
    let m = parseByteString parseHeader mempty headerEx
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "blah")
    
  describe "Comment parsing" $
    it "Can skip a comment before a header" $ do
    let p = skipComments >> parseHeader
        i = "; woot\n[blah]"
        m = parseByteString p mempty i
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "blah")

  describe "Section parsing" $
    it "Can parse a simple section" $ do
    let m = parseByteString parseSection mempty sectionEx
        r' = maybeSuccess m
        states = M.fromList [("Chris", "Texas")]
        expected' = Just (Section (Header "states") states)
    print m
    r' `shouldBe` expected'

  describe "INI parsing" $
    it "Can parse multiple sections" $ do
    let m = parseByteString parseIni mempty sectionEx''
        r' = maybeSuccess m
        sectionValues = M.fromList
                        [("alias", "claw") , ("host", "wikipedia.org")]
        whatisitValues = M.fromList [("red", "intoothandclaw")]
        expected' = Just (Config
                          (M.fromList
                           [(Header "section" , sectionValues) ,
                            (Header "whatisit" , whatisitValues)]))
    print m
    r' `shouldBe` expected'    
