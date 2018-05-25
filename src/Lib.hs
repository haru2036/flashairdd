{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Network.HTTP.Client
import Data.Text
import Data.Text.Lazy.Encoding
import Data.ByteString(ByteString)
import Data.ByteString.Lazy(toStrict)

someFunc = requestTop

getUrl :: String -> IO ByteString
getUrl url = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    return $ toStrict $ responseBody response
    

requestTop :: IO ()
requestTop = getUrl "192.168.1.105" >>= print 

appendBaseUrl :: String -> String
appendBaseUrl = (++) "http://192.168.1.105/" 