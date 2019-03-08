import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 n g
  where go sum count limit gen
          | sum >= limit = count
          | otherwise =
            let (rn, st) = randomR (1, 6) gen
            in go (sum + rn) (count + 1) limit st

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] n g
  where go sum count recd limit gen
          | sum >= limit = (count, recd)
          | otherwise =
            let (rn, st) = randomR (1, 6) gen
            in go (sum + rn) (count + 1) (recd ++ [intToDie rn]) limit st
                 
main :: IO ()
main = undefined
