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
data LocalPath = LDirPath FilePath | LJPGPath FilePath deriving(Show)


class Path a where
    getFullPath :: a -> FilePath
    getPath :: a -> FilePath

instance Path FAPath where
    getFullPath (DirPath path) = appendBaseUrl path
    getFullPath (JPGPath path) = appendBaseUrl path
    getPath (DirPath path) = path
    getPath (JPGPath path) = path

instance Path LocalPath where
    getFullPath (LDirPath path) = appendBaseUrl path
    getFullPath (LJPGPath path) = appendBaseUrl path
    getPath (LDirPath path) = path
    getPath (LJPGPath path) = path

requestTop :: IO ()
requestTop = do
    result <- getUrl (appendBaseUrl "") 
    print $ parseFileList $ unpack result

downloadAllFiles :: IO()
downloadAllFiles = do
    fileList <- listFAFiles
    mapM_ downloadAndSaveFile fileList
    return ()

getUrl :: String -> IO ByteString
getUrl url = do
    manager  <- newManager defaultManagerSettings
    request  <- parseRequest url
    response <- httpLbs request manager
    return $ toStrict $ responseBody response

appendBaseUrl :: String -> String
appendBaseUrl = (++) "http://192.168.1.105"

parseFileList :: String -> [JSONFileInfo]
parseFileList body = mapMaybe parseWlanPush $ findWlanPush body

walkDir :: (FAPath -> IO [FAPath]) -> FAPath -> IO [FAPath]
walkDir list (JPGPath path) = return [JPGPath path]
walkDir list (DirPath path) = do
    paths <- list $ DirPath path
    walkd <- mapM (walkDir list) paths 
    return $ concat walkd

downloadAndSaveFile :: FAPath -> IO()
downloadAndSaveFile path = do
    result <- openURI $ getFullPath path
    case result of 
        Right content -> do 
            print ("downloading:" ++ (fileName (getPath path)))
            createDirectoryIfMissing True $ fileDir ("photos" ++ getPath path)
            Data.ByteString.writeFile ("photos" ++ getPath path) content
        Left err -> print err
        where fileDir = reverse . dropWhile ('/' /=) . reverse
              fileName = reverse . takeWhile('/' /=) . reverse

listFAFiles :: IO [FAPath]
listFAFiles = walkDir listFA $ DirPath "/" 

listFA :: FAPath -> IO [FAPath]
listFA p = do
    result <- getUrl $ getFullPath p
    return $ map convertToFAPath $ parseFileList $ unpack result


convertToFAPath :: JSONFileInfo -> FAPath
convertToFAPath (JSONFileInfo uri fname fsize attr fdate ftime)
    | fsize == 0 = DirPath $ uri ++ "/" ++ fname
    | otherwise = JPGPath $ uri ++ "/" ++ fname
