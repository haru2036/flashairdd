{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    , getUrl
    , appendBaseUrl
    )
where

import           Network.HTTP.Client
import           Network.Download
import           Control.Monad
import           Data.Text                      ( Text )
import           Data.Text.Lazy.Encoding
import           Data.ByteString                ( ByteString
                                                , writeFile
                                                )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.ByteString.Char8          ( unpack )
import           Data.List.Split                ( splitOn )
import           Data.List                      ( filter
                                                , notElem
                                                )
import           System.Directory
import           Pages
import           Types
import           FAPath
import           LocalPath
import           HTTP

someFunc = downloadNotDownloadedFiles




downloadAllFiles :: IO ()
downloadAllFiles = do
    fileList <- listFAFiles
    mapM_ downloadAndSaveFile fileList
    return ()

downloadNotDownloadedFiles :: IO ()
downloadNotDownloadedFiles = do
    faFileList <- listFAFiles
    lFileList  <- listLocalFiles
    let lfiles = map getPath lFileList
    print lfiles
    mapM_ downloadAndSaveFile
        $ filter (\item -> getPath item `notElem` lfiles) faFileList


downloadAndSaveFile :: FAPath -> IO ()
downloadAndSaveFile path = do
    result <- openURI $ appendBaseUrl $ getPath path
    case result of
        Right content -> do
            print ("downloading:" ++ (fileName (getPath path)))
            createDirectoryIfMissing True $ fileDir ("photos" ++ getPath path)
            Data.ByteString.writeFile ("photos" ++ getPath path) content
        Left err -> print err
  where
    fileDir  = reverse . dropWhile ('/' /=) . reverse
    fileName = reverse . takeWhile ('/' /=) . reverse

