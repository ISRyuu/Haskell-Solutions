import System.IO
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Applicative
import Data.Functor.Identity


rDec :: Num a => Reader a a
rDec = ReaderT $ \a -> Identity (a - 1)

-- point free version of rDec
pfrDec :: Num a => Reader a a
pfrDec = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \a -> Identity (show a)

-- point free version of rShow
pfrShow :: Show a => ReaderT a Identity String
pfrShow = reader $ show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
  liftIO (putStrLn $ "Hi: " ++ show (a + 1))
  return (a + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  let z = show s
  liftIO (putStrLn $ "Hi: " ++ z)
  return (z, s+1)
