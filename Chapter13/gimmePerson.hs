import Control.Monad (forever)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                 | AgeTooLow
                 | PersonInvalidUnknown String
                 deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                "Name was: " ++ show name ++
                " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = forever $ do
  putStr "Input name: "
  name <- getLine
  putStr "Input age: "
  age <- getLine
  case (mkPerson name (read age :: Integer)) of
    Left e -> putStrLn $ "error " ++ show e
    Right p -> print p
