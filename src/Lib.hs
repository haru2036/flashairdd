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
import           Data.List                      ( filter, notElem)
import           Pages

someFunc = downloadNotDownloadedFiles 

data FAPath = DirPath FilePath | JPGPath FilePath deriving(Show)
data LocalPath = LDirPath FilePath | LJPGPath FilePath deriving(Show)


class Path a where
    getPath :: a -> FilePath
    list :: a -> IO [a] 

instance Path FAPath where
    getPath (DirPath path) = path
    getPath (JPGPath path) = path
    list p@(DirPath path) = do
        result <- getUrl $ appendBaseUrl path
        return $ map convertToFAPath $ parseFileList $ unpack result
    list p@(JPGPath path) = return [p]


instance Path LocalPath where
    getPath (LDirPath path) = path
    getPath (LJPGPath path) = path
    list (LDirPath path) = do
        directories <- listDirectory path
        mapM isDirectoryLocal directories
    list p@(LJPGPath path) = return [p]

isDirectoryLocal :: FilePath -> IO LocalPath 
isDirectoryLocal path = do
    print path 
    isdir <- doesDirectoryExist path
    return $ case isdir of 
        True -> LDirPath path
        False -> LJPGPath path

requestTop :: IO ()
requestTop = do
    result <- getUrl (appendBaseUrl "") 
    print $ parseFileList $ unpack result

downloadAllFiles :: IO()
downloadAllFiles = do
    fileList <- listFAFiles
    mapM_ downloadAndSaveFile fileList
    return ()

downloadNotDownloadedFiles :: IO()
downloadNotDownloadedFiles = do
    faFileList <- listFAFiles
    lFileList <- listLocalFiles 
    let lfiles = map getPath lFileList
    print lfiles
    mapM_ downloadAndSaveFile $ filter (\item -> getPath item `notElem` lfiles) faFileList

listFAFiles :: IO [FAPath]
listFAFiles = walkDir list $ DirPath "/" 

listLocalFiles :: IO [LocalPath]
listLocalFiles = do
    cd <- getCurrentDirectory 
    walkDirLocal list $ LDirPath $ cd ++ "/photos"
getUrl :: String -> IO ByteString
getUrl url = do
    manager  <- newManager defaultManagerSettings
    request  <- parseRequest url
    response <- httpLbs request manager
    return $ toStrict $ responseBody response

appendBaseUrl :: String -> String
appendBaseUrl = (++) "http://192.168.0.1"

parseFileList :: String -> [JSONFileInfo]
parseFileList body = mapMaybe parseWlanPush $ findWlanPush body

walkDir :: (FAPath -> IO [FAPath]) -> FAPath -> IO [FAPath]
walkDir list (JPGPath path) = return [JPGPath path]
walkDir list (DirPath path) = do
    paths <- list $ DirPath path
    walkd <- mapM (walkDir list) paths 
    return $ concat walkd

walkDirLocal :: (LocalPath -> IO [LocalPath]) -> LocalPath -> IO [LocalPath]
walkDirLocal list (LJPGPath path) = return [LJPGPath path]
walkDirLocal list (LDirPath path) = do
    paths <- list $ LDirPath path
    walkd <- mapM (walkDirLocal list) paths 
    print walkd
    return $ concat walkd

downloadAndSaveFile :: FAPath -> IO()
downloadAndSaveFile path = do
    result <- openURI $ appendBaseUrl $ getPath path
    case result of 
        Right content -> do 
            print ("downloading:" ++ (fileName (getPath path)))
            createDirectoryIfMissing True $ fileDir ("photos" ++ getPath path)
            Data.ByteString.writeFile ("photos" ++ getPath path) content
        Left err -> print err
        where fileDir = reverse . dropWhile ('/' /=) . reverse
              fileName = reverse . takeWhile('/' /=) . reverse



convertToFAPath :: JSONFileInfo -> FAPath
convertToFAPath (JSONFileInfo uri fname fsize attr fdate ftime)
    | fsize == 0 = DirPath $ uri ++ "/" ++ fname
    | otherwise = JPGPath $ uri ++ "/" ++ fname
