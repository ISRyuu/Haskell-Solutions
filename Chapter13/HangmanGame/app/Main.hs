module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


newtype WordList = WordList [String]
data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle s d g) =
    (intersperse ' ' $ fmap renderPuzzleChar d)
    ++ " Guessed so far: " ++ g ++ " the answer is: " ++ s

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

maxWordLength :: Int
maxWordLength = 9

minWordLength :: Int
minWordLength = 5

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  i <- randomRIO (0, (length wl - 1))
  return (wl !! i)

randomWord' :: IO String
randomWord' = gameWords >>= randomWord
  
freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@(Puzzle s g d) c
  | c `elem` d = Puzzle s g d
  | c `elem` s  = Puzzle s (zipWith (go c) s g) (c:d)
  | otherwise = Puzzle s g (c:d)
  where go c x y = if c == x then Just x else y

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess p c
  | alreadyGuessed p c = do
      putStrLn "You already guessed that character,\
            \ pick something else."
      return p
  | not $ charInWord p c = do
      putStrLn "This character wasn't in the word,\
               \ try again"
      return (fillInCharacter p c)
  | otherwise = do
      putStrLn "This character was in the word,\
               \ filling the word accordingly"
      return (fillInCharacter p c)
      
gameOver :: Puzzle -> IO ()
gameOver (Puzzle s _ g) =
  if (length $ filter (not . (`elem`s)) g) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ s
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle s d _) =
  if all isJust d then
    do putStrLn "You win!"
       putStrLn $ "The word was: " ++ s
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame 
    _ -> putStrLn "You must input a single letter." 
    
  
main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
