{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , getUrl
    , appendBaseUrl
    )
where

import           Network.HTTP.Client
import           Control.Monad
import           Data.Text                      ( Text )
import           Data.Text.Lazy.Encoding
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.ByteString.Char8          ( unpack )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( mapMaybe)
import           Pages

someFunc = listFiles >>= print

data FAPath = DirPath FilePath | JPGPath FilePath deriving(Show)

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

listFiles :: IO [FAPath]
listFiles = walkDir $ DirPath "/"

appendBaseUrl :: String -> String
appendBaseUrl = (++) "http://192.168.1.105"

parseFileList :: String -> [JSONFileInfo]
parseFileList body = mapMaybe parseWlanPush $ findWlanPush body

walkDir :: FAPath -> IO [FAPath]
walkDir (JPGPath path) = return [JPGPath path]
walkDir (DirPath path) = do
    paths <- list $ DirPath path
    print paths
    walkd <- mapM walkDir paths
    return $ concat walkd

list :: FAPath -> IO [FAPath]
list (DirPath path) = do
    result <- getUrl $ appendBaseUrl path
    return $ map convertToFAPath $ parseFileList $ unpack result
list (JPGPath path) = do
    result <- getUrl $ appendBaseUrl path
    print path
    return $ map convertToFAPath $ parseFileList $ unpack result
 
convertToFAPath :: JSONFileInfo -> FAPath
convertToFAPath (JSONFileInfo uri fname fsize attr fdate ftime)
    | fsize == 0 = DirPath $ uri ++ "/" ++ fname
    | otherwise = JPGPath $ uri ++ "/" ++ fname
