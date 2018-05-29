{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , getUrl
    , appendBaseUrl
    )
where

import           Network.HTTP.Client
import           Network.Download
import           System.Directory
import           Control.Monad
import           Data.Text                      ( Text )
import           Data.Text.Lazy.Encoding
import           Data.ByteString                ( ByteString, writeFile)
import           Data.ByteString.Lazy           ( toStrict )
import           Data.ByteString.Char8          ( unpack )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( mapMaybe)
import           Pages

someFunc = downloadAllFiles

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

downloadAllFiles :: IO()
downloadAllFiles = do
    fileList <- listFiles
    mapM downloadAndSaveFile fileList
    return ()

getFullPath :: FAPath -> FilePath
getFullPath (DirPath path) = appendBaseUrl path
getFullPath (JPGPath path) = appendBaseUrl path

getPath :: FAPath -> FilePath
getPath (DirPath path) = path
getPath (JPGPath path) = path

downloadAndSaveFile :: FAPath -> IO()
downloadAndSaveFile path = do
    result <- openURI $ getFullPath path
    case result of 
        Right content -> do 
            print ("downloading:" ++ (fileName (getPath path)))
            createDirectoryIfMissing True $ fileDir ("photos" ++ getPath path)
            Data.ByteString.writeFile ("photos" ++ getPath path) content
        Left err -> print err
        where fileDir = reverse . (dropWhile ('/' /=)) . reverse
              fileName = reverse . (takeWhile('/' /=)) . reverse


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
list p@(DirPath path) = do
    result <- getUrl $ getFullPath p
    return $ map convertToFAPath $ parseFileList $ unpack result
list p@(JPGPath path) = do
    result <- getUrl $ getFullPath p
    print path
    return $ map convertToFAPath $ parseFileList $ unpack result
 
convertToFAPath :: JSONFileInfo -> FAPath
convertToFAPath (JSONFileInfo uri fname fsize attr fdate ftime)
    | fsize == 0 = DirPath $ uri ++ "/" ++ fname
    | otherwise = JPGPath $ uri ++ "/" ++ fname
