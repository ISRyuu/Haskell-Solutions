import Data.List (group, sort, elemIndex)
import Data.Char (isUpper, toLower)
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
fingerTaps = foldr (\(_, x) b -> x + b) 0

mostPopularElem :: (Eq a, Ord a) => [a] -> (Int, a)
mostPopularElem = maximum . map (\x -> (length x, head x)) . group . sort

mostPopularLetterCost :: DaPhone -> String -> Int
mostPopularLetterCost p = go . mostPopularElem
  where
    go (n, c) = n * (foldr ((+) . snd) 0 $ (reverseTaps p c))

coolestLtr :: [String] -> Char
coolestLtr = snd . maximum . map mostPopularElem

coolestWord :: [String] -> String
coolestWord = snd . maximum . map (mostPopularElem . words)

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


main = do
  let x = map (cellPhonesDead $ phone) convo
      phone = makeDaPhone myPhone
  print x
  print $ map fingerTaps x
  print $ map mostPopularElem convo
  print $ map (mostPopularLetterCost phone) convo
  print $ coolestLtr convo
  print $ coolestWord convo  
