import Text.Trifecta

oneTwoEof = char '1' >> char '2' >> eof

--- I haven't solved problem 2.

myString :: CharParsing m => String -> m String
myString [] = pure []
myString (x:xs) = (:) <$> char x <*> myString xs

  
main :: IO ()
main = do
  print $ parseString (myString "123") mempty "123"
  print $ parseString oneTwoEof mempty "123"
