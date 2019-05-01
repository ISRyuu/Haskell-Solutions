{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.String (fromString)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    count :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  (M.insert k x m, x)
  where x =
          case M.lookup k m of
            Nothing -> 1
            Just x -> x + 1
    
app :: Scotty ()
app =
  get "/:key" $ do
  (unprefixed :: Text) <- param "key"
  config <- lift $ ReaderT return
  let key' = mappend (prefix config) unprefixed
      dictRef = count config
  (newRef, newInteger) <- liftIO $ bumpBoomp key' <$> (readIORef $ dictRef)
  liftIO $ writeIORef dictRef newRef
  html $ mconcat [
       "<h1>Success! Count was:"
     , TL.pack $ show newInteger
     , "</h1>"
     ]
  
main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ fromString prefixArg
      runR r = runReaderT r config
  scottyT 3001 runR app
