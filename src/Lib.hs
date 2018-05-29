{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , getUrl
    , appendBaseUrl
    )
where

import           Network.HTTP.Client
import           Data.Text                      ( Text )
import           Data.Text.Lazy.Encoding
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.ByteString.Char8          ( unpack )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( mapMaybe)
import           Pages

someFunc = requestTop

getUrl :: String -> IO ByteString
getUrl url = do
    manager  <- newManager defaultManagerSettings
    request  <- parseRequest url
    response <- httpLbs request manager
    return $ toStrict $ responseBody response


requestTop :: IO ()
requestTop = do
    result <- getUrl (appendBaseUrl "") 
    print $ parseFileList $ unpack result

appendBaseUrl :: String -> String
appendBaseUrl = (++) "http://192.168.1.105/DCIM"

parseFileList :: String -> [JSONItem]
parseFileList body = mapMaybe parseWlanPush $ findWlanPush body

