{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Web.Scotty
import Data.Maybe
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Concurrent


main :: IO ()
main = do
  downloadThread <- newMVar Nothing
  scotty 3000 $ do
    get "/download" $ do
      downloading <- liftIO $ takeMVar downloadThread
      liftIO $ case downloading of
        Just threadId -> killThread threadId
        Nothing -> return ()
      liftIO $ do
        threadId <- forkIO someFunc 
        putMVar downloadThread $ Just threadId
      html $ mconcat ["<h1>download started</h1>"]

    get "/cancel" $ do
      html $ mconcat ["<h1>canceling download</h1>"]
      liftIO $ do
        downloading <- takeMVar downloadThread
        case downloading of
          Just threadId -> killThread threadId
          Nothing -> return ()
        putMVar downloadThread $ Nothing