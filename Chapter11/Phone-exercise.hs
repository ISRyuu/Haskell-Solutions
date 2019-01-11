import Data.List
import Data.Char
import System.IO


type Phone = [String]

myPhone :: Phone
myPhone = [
  "1"    , "2abc" , "3def",
  "4ghi" , "5jkl" , "6mno",
  "7pqrs", "8tuv" , "9wxyz",
  "*^"   , "0+ _" , "#.,"
        ] 

data Botton = Botton {getDigit :: Char,  getValue :: [Char]} deriving Show
data DaPhone = DaPhone [Botton] deriving Show

makeDaPhone :: Phone -> DaPhone
makeDaPhone = DaPhone . map toButton
  where toButton b@(x:_) = Botton x b

type Digit = Char
type Press = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Press)]
reverseTaps (DaPhone []) _ = [] -- ingore undefined key
reverseTaps (DaPhone (b:bs)) c
  | isUpper c = ('*', 1) : reverseTaps (DaPhone bs) (toLower c)
  | otherwise = 
    case c `elemIndex` (getValue b) of
      Nothing -> reverseTaps (DaPhone bs) c
      Just n  -> [(getDigit b, n+1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Press)]
cellPhonesDead d = concat . map (reverseTaps d)

fingerTaps :: [(Digit, Press)] -> Press
fingerTaps = undefined
  
convo :: [String]
convo = [
  "Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"
  ]


main = print $ map (cellPhonesDead $ makeDaPhone myPhone) convo
