import Control.Monad
import qualified Text.Read as TR
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import System.Console.ANSI
import System.IO
import System.Random
import qualified Data.Map as M

data ScoreBoard = ScoreBoard
  { getUserScore :: Int
  , getBotScore :: Int
  }
  
instance Show ScoreBoard where
  show bd =
    concat ["score user1: "
           , show $ getUserScore bd
           , " user2: "
           , show $ getBotScore bd]

userWin :: ScoreBoard -> Bool
userWin = (10 <=) . getUserScore

botWin :: ScoreBoard -> Bool
botWin = (10 <=) . getBotScore

-- User is evens
-- Computer is odds
gameRound :: ScoreBoard -> Int -> ScoreBoard
gameRound b s = case even s of
  True -> ScoreBoard (getUserScore b + 1) (getBotScore b)
  _    -> ScoreBoard (getUserScore b) (getBotScore b + 1)

game :: StateT ScoreBoard IO ()
game = do
  board <- get
  sum <- liftIO guessGame
  let newBoard = gameRound board sum
  put newBoard
  liftIO . print $ newBoard
  gameOver newBoard
    where gameOver bd
            | userWin bd = liftIO . putStrLn $ "user1 win"
            | botWin  bd = liftIO . putStrLn $ "user2 win"
            | otherwise = game

inRange :: Maybe Int -> Maybe Int
inRange (Just 1) = Just 1
inRange (Just 2) = Just 2
inRange _        = Nothing

userInput :: IO Int
userInput = do
  putStrLn "guess a number(1 or 2): "  
  a <- TR.readMaybe <$> getLine
  let z = inRange a
  case z of
    Nothing -> putStrLn "invalid input!" >> userInput
    Just x -> return x
  
guessGame :: IO Int
guessGame = do
  a <- userInput
  clearScreen
  b <- userInput
  return $ a + b

main :: IO ()
main = do
  evalStateT game $ ScoreBoard {getUserScore = 0, getBotScore = 0}
